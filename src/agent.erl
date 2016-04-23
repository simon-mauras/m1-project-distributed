-module(agent).
-export([node_init/0, node_init/1, start/0]).

% ================================ Utilities ================================= %

-record(meta, {size, load}).
-record(treap, {up, left, right,
                meta, meta_left, meta_right,
                jobs, job_token, value}).

-define(INFINITY, 1.0e10).
-define(WAITING_TIME, 1000).

init_seed() ->
  random:seed(erlang:phash2([node(), self()]), 42, 42).
  

% ================================== Node ==================================== %

% Update metadata (from a list of metadata, children)
meta_update() -> #meta{ size = 1, load = 0 }.
meta_update([]) -> meta_update();
meta_update([undefined|T]) -> meta_update(T);
meta_update([H|T]) ->
  Result = meta_update(T),
  Result#meta{
    size = Result#meta.size + H#meta.size
  }.

% Update the load of a subtree
meta_load(Meta, Nb) ->
  Meta#meta{ load = Meta#meta.load + Nb / Meta#meta.size }.

% Find the root of the tree
node_root(Treap, Return) ->
  case Treap#treap.up of
    undefined -> Return ! { root_end, self() };
    Up -> Up ! { root, Return }
  end,
  node_controller(Treap).

% Say hello (debug)
node_say_hello(Treap) ->
  io:fwrite("Hi! I'm ~w(~w).~n", [self(), Treap#treap.value]),
  case Treap#treap.left of undefined -> ok; PidL -> PidL ! { ping } end,
  case Treap#treap.right of undefined -> ok; PidR -> PidR ! { ping } end,
  node_controller(Treap).

% Spawn the next job.
node_job_next(Treap, Token) ->
  case { Treap#treap.job_token + Token, Treap#treap.jobs } of
    { T, [ { Module, Function, Args, Return } | J ] }  when T > 0->
      Pid = self(),
      F = fun() ->
        Return ! apply(Module, Function, Args),
        Pid ! { job_load, { -1, undefined } },
        Pid ! { job_next, 1 }
      end,
      spawn(F),
      node_controller(Treap#treap{job_token = T-1, jobs = J});
    { T, _ } ->
      node_controller(Treap#treap{job_token = T})
  end.

% Update load of the subtree
node_job_load(Treap, { Nb, Child }) ->
  case Treap#treap.up of
    undefined -> ok;
    Up -> Up ! { job_load, { Nb, self() } }
  end,
  PidLeft = Treap#treap.left,
  PidRight = Treap#treap.right,
  case Child of
    undefined -> 
      node_controller(Treap#treap{
        meta = meta_load(Treap#treap.meta, Nb)
      });
    PidLeft ->
      node_controller(Treap#treap{
        meta = meta_load(Treap#treap.meta, Nb),
        meta_left = meta_load(Treap#treap.meta_left, Nb)
      });
    PidRight ->
      node_controller(Treap#treap{
        meta = meta_load(Treap#treap.meta, Nb),
        meta_right = meta_load(Treap#treap.meta_right, Nb)
      })
  end.

% Add a new job to the queue.
node_job_new(Treap, { Root, Threshold, Job }) ->
  case { Root, Treap#treap.up } of
    { _, undefined } -> ok;
    { false, _} -> ok;
    { true, Up } -> Up ! { job_load, { 1, self() } }
  end,
  LoadNode = Treap#treap.meta#meta.load,
  LoadLeft = case Treap#treap.meta_left of
    undefined -> ?INFINITY;
    MetaLeft -> MetaLeft#meta.load
  end,
  LoadRight = case Treap#treap.meta_right of
    undefined -> ?INFINITY;
    MetaRight -> MetaRight#meta.load
  end,
  Return = (LoadNode < LoadRight) and (LoadNode < LoadLeft),
  case (Treap#treap.meta#meta.size < Threshold) and Return of
    true ->
      NewTreap = Treap#treap{
        meta = meta_load(Treap#treap.meta, 1),
        jobs = [Job|Treap#treap.jobs]
      },
      self() ! { job_next, 0 },
      node_controller(NewTreap);
    false ->
      case LoadLeft < LoadRight of
        true ->
          Treap#treap.left ! { job_new, { false, Threshold, Job } },
          NewTreap = Treap#treap{
            meta = meta_load(Treap#treap.meta, 1),
            meta_left = meta_load(Treap#treap.meta_left, 1)
          },
          node_controller(NewTreap);
        false ->
          Treap#treap.right ! { job_new, { false, Threshold, Job } },
          NewTreap = Treap#treap{
            meta = meta_load(Treap#treap.meta, 1),
            meta_right = meta_load(Treap#treap.meta_right, 1)
          },
          node_controller(NewTreap)
      end
  end.

% Split the tree according to the threshold.
node_split(Treap, { Threshold, UpLeft, UpRight, Return }) ->
  Size = case Treap#treap.meta_left of
    undefined -> 0;
    Meta -> Meta#meta.size
  end,
  case Threshold - Size - 1 of
  X when X < 0 ->
    case Treap#treap.left of
    undefined ->
      Return ! { split_end, undefined, self(), undefined, Treap#treap.meta },
      node_controller(Treap#treap{ up = UpRight });
    PidLeft ->
      PidLeft ! { split, { Threshold, UpLeft, self(), self() } },
      receive { split_end, Left, Right, MetaLeft, MetaRight } ->
        NewTreap = Treap#treap{
          up = UpRight,
          left = Right,
          meta_left = MetaRight,
          meta = meta_update([MetaRight, Treap#treap.meta_right])
        },
        Return ! { split_end, Left, self(), MetaLeft, NewTreap#treap.meta },
        node_controller(NewTreap)
      end
    end;
  X when X >= 0 ->
    case Treap#treap.right of
    undefined ->
      Return ! { split_end, self(), undefined, Treap#treap.meta, undefined },
      node_controller(Treap#treap{ up = UpLeft });
    PidRight ->
      PidRight ! { split, { X, self(), UpRight, self() } },
      receive { split_end, Left, Right, MetaLeft, MetaRight } ->
        NewTreap = Treap#treap{
          up = UpLeft,
          right = Left,
          meta_right = MetaLeft,
          meta = meta_update([Treap#treap.meta_left, MetaLeft])
        },
        Return ! { split_end, self(), Right, NewTreap#treap.meta, MetaRight },
        node_controller(NewTreap)
      end
    end
  end.

% Merge the 2 substrees.
node_merge(Treap, { _, undefined, Up, Return } ) ->
  Return ! { merge_end, self(), Treap#treap.meta },
  node_controller(Treap#treap{ up = Up });
node_merge(Treap, { Side, Pid, Up, Return }) ->
  Pid ! { merge_begin, self(), Treap#treap.value, Treap#treap.meta },
  receive { merge_begin, Pid, Value, Meta } ->
    case Value < Treap#treap.value of
    true ->
      Return ! { merge_end, self(), meta_update([
        Meta,
        Treap#treap.meta_left,
        Treap#treap.meta_right
      ]) },
      case Side of
      left  ->
        Pid ! { merge_continue, Treap#treap.right },
        case Treap#treap.right of
          undefined -> ok;
          R  -> R ! { merge, { left, Pid, self(), self() } }
        end,
        receive { merge_end, PidChild, MetaChild } ->
          node_controller(Treap#treap{
            up = Up,
            right = PidChild,
            meta_right = MetaChild,
            meta = meta_update([MetaChild, Treap#treap.meta_left])
          })
        end;
      right ->
        Pid ! { merge_continue, Treap#treap.left },
        case Treap#treap.left of
          undefined -> ok;
          L  -> L ! { merge, { right, Pid, self(), self() } }
        end,
        receive { merge_end, PidChild, MetaChild } -> 
          node_controller(Treap#treap{
            up = Up,
            left = PidChild,
            meta_left = MetaChild,
            meta = meta_update([MetaChild, Treap#treap.meta_right])
          })
        end
      end;
    false ->
      receive { merge_continue, NewPid } ->
        node_merge(Treap, {Side, NewPid, Pid, Pid} )
      end
    end
  end.

% Return the list of nodes of the treap
node_ping(Treap, { Acc1, Return }) ->
  Acc2 = case Treap#treap.left of
    undefined -> Acc1;
    PidL -> PidL ! { ping, { Acc1, self() } },
            receive { ping_end, Res1 } -> Res1 end
  end,
  Acc3 = case Treap#treap.right of
    undefined -> Acc2;
    PidR -> PidR ! { ping, { Acc2, self() } },
            receive { ping_end, Res2 } -> Res2 end
  end,
  Return ! { ping_end, [ self() | Acc3 ] },
  node_controller(Treap).

% Print the topology in a dot file.
node_graph(Treap, { File, Return }) ->
  io:fwrite(File, "~w [label=\"~w\\n~w\"];~n", [self(), self(), Treap#treap.meta]),
  case Treap#treap.up of
    undefined -> ok;
    PidU -> io:fwrite(File, "~w -> ~w [style=dashed];~n", [self(), PidU])
  end,
  case Treap#treap.left of
    undefined -> ok;
    PidL -> io:fwrite(File, "~w -> ~w;~n", [self(), PidL]),
            PidL ! { graph, { File, self() } },
            receive { graph_end } -> ok after ?WAITING_TIME -> ok end
  end,
  case Treap#treap.right of
    undefined -> ok;
    PidR -> io:fwrite(File, "~w -> ~w;~n", [self(), PidR]),
            PidR ! { graph, { File, self() } },
            receive { graph_end } -> ok after ?WAITING_TIME -> ok end
  end,
  Return ! { graph_end },
  node_controller(Treap).

% Branching function of the node.
node_controller(Treap) ->
  receive
    { say_hello } -> node_say_hello(Treap);
    { ping, Data } -> node_ping(Treap, Data);
    { job_load, Data } -> node_job_load(Treap, Data);
    { job_new, Data } -> node_job_new(Treap, Data);
    { job_next, Data } -> node_job_next(Treap, Data);
    { root, Data } -> node_root(Treap, Data);
    { graph, Data } -> node_graph(Treap, Data);
    { split, Data } -> node_split(Treap, Data);
    { merge, Data } -> node_merge(Treap, Data);
    { kill } -> ok %io:fwrite("~w: I'm dead~n", [self()])
  end.

% Init function (initialize the node as a treap of size 1)
node_init() -> node_init(1).
node_init(JobToken) ->
  %io:fwrite("~w: I'm alive~n", [self()]),
  init_seed(),
  node_controller(#treap{
    jobs = [],
    job_token = JobToken,
    value = random:uniform(1 bsl 30),
    meta = meta_update()
  }).

start() ->
  erlang:register(agent, self()),
  node_init(1).
