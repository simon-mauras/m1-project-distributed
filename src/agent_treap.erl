-module(agent_treap).
-export([node_init/0, init_seed/0, spawns/2,
         ask_merge/2, ask_split/2, ask_job/4,
         ask_root/1, ask_graph/2, ask_ping/1,
         test/1]).

-record(meta, {size, load}).
-record(treap, {up, left, right,
                meta, meta_left, meta_right,
                jobs, job_token, value}).

-define(WAITING_TIME, 1000).
-define(INFINITY, 1.0e10).

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

meta_load(Meta, Nb) ->
  Meta#meta{ load = Meta#meta.load + Nb / Meta#meta.size }.

% Find the root of the tree
node_root(Treap, Return) ->
  case Treap#treap.up of
    undefined -> Return ! { root_end, self() };
    Up -> Up ! { root, Return }
  end,
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
  X when X =< 0 ->
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
  X when X > 0 ->
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

node_ping(Treap) ->
  io:fwrite("Hi! I'm ~w(~w).~n", [self(), Treap#treap.value]),
  case Treap#treap.left of undefined -> ok; PidL -> PidL ! { ping } end,
  case Treap#treap.right of undefined -> ok; PidR -> PidR ! { ping } end,
  node_controller(Treap).

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

node_controller(Treap) ->
  receive
    { ping } -> node_ping(Treap);
    { job_load, Data } -> node_job_load(Treap, Data);
    { job_new, Data } -> node_job_new(Treap, Data);
    { job_next, Data } -> node_job_next(Treap, Data);
    { root, Data } -> node_root(Treap, Data);
    { graph, Data } -> node_graph(Treap, Data);
    { split, Data } -> node_split(Treap, Data);
    { merge, Data } -> node_merge(Treap, Data);
    { kill } -> io:fwrite("~w: I'm dead~n", [self()])
  end.

node_init() ->
  io:fwrite("~w: I'm alive~n", [self()]),
  init_seed(),
  node_controller(#treap{
    jobs = [],
    job_token = 2,
    value = random:uniform(1 bsl 30),
    meta = meta_update()
  }).


% ================================ Utilities ================================= %
spawns(0, _) -> [];
spawns(N, Fun) -> [spawn(Fun)|spawns(N-1,Fun)].

init_seed() ->
  random:seed(erlang:phash2([node(), self()]), 42, 42).

% ================================== Test ==================================== %
ask_merge(undefined, Pid) -> Pid;
ask_merge(Pid, undefined) -> Pid;
ask_merge(Pid1, Pid2) ->
  Pid1 ! { merge, { left, Pid2, undefined, self() } },
  Pid2 ! { merge, { right, Pid1, undefined, self() } },
  receive { merge_end, Pid, _ } ->
    %io:fwrite("Merge(~w,~w) = ~w~n", [Pid1, Pid2, Pid]),
    Pid
  end.

ask_split(Pid, Threshold) ->
  Pid ! { split, { Threshold, undefined, undefined, self() } },
  receive { split_end, Pid1, Pid2, _, _ } ->
    { Pid1, Pid2 }
  end.

ask_ping(Pid) -> Pid ! { ping }, ok.

ask_graph(Pid, Filename) ->
  { ok, File } = file:open(Filename, [write]),
  io:fwrite(File, "digraph topology { ~n", []),
  Pid ! { graph, { File, self() } },
  receive { graph_end } -> ok after ?WAITING_TIME -> ok end,
  io:fwrite(File, "}~n", []),
  file:close(File).

ask_root(Pid) ->
  Pid ! { root, self() },
  receive { root_end, Root } -> Root end.

ask_job(Pid, Module, Function, Args) ->
  Pid ! { job_new, { true, 1000, { Module, Function, Args, self() } } }.

start() ->
  register(client, self()),
  node_init().

test(N) ->
  Pids = spawns(N, fun node_init/0),
  Pid1 = lists:foldl(fun ask_merge/2, undefined, Pids),
  ask_graph(Pid1, "graph1.dot"),
  { Pid2, Pid3 } = ask_split(Pid1, N div 2),
  ask_graph(Pid2, "graph2-1.dot"),
  ask_graph(Pid3, "graph2-2.dot"),
  Pid = ask_merge(Pid2, Pid3),
  Args = lists:map(fun (_) -> [1000] end, lists:seq(1, 10*N)),
  lists:map(fun (Arg) -> ask_job(Pid2, jobs, wait, Arg) end, Args),
  ask_graph(Pid, "graph3.dot"),
  lists:map(fun (_) -> receive ok -> ok end end, Args),
  ask_graph(Pid, "graph4.dot"),
  lists:map(fun (P) -> P ! { kill } end, Pids),
  ok.
