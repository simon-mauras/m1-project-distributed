-module(agent).
-export([network_create/1, node_join/1, test_network/1, test_join/0]).

-record(topology, {id, nb_nodes, neighbours}).

% ========= Tools ========== %
get_neighbours(_,  Nb, K) when (1 bsl K) >= Nb -> [];
get_neighbours(Id, Nb, K) when (1 bsl K) bxor Id >= Nb ->
  get_neighbours(Id, Nb, K+1);
get_neighbours(Id, Nb, K) ->
  [Id bxor (1 bsl K)|get_neighbours(Id, Nb, K+1)].
get_neighbours(Id, Nb) ->
  get_neighbours(Id, Nb, 0).

spawns(0, _) -> [];
spawns(N, Fun) -> [spawn(Fun)|spawns(N-1,Fun)].

pop_count(0, X) -> X;
pop_count(N, X) -> pop_count(N div 2, N rem 2 + X).
pop_count(N) -> pop_count(N, 0).

sleep(N) -> receive after N -> ok end.

% ===== Node receive ===== %
node_receive(Topology, print_yourself) ->
  io:fwrite("~w: ~w~n", [self(), Topology]),
  node_controller(Topology);
node_receive(Topology, { i_am_alive, Pid }) ->
  Pid ! { Topology#topology.nb_nodes, Topology#topology.nb_nodes+1 },
  node_broadcast({ he_is_alive, Topology#topology.nb_nodes, Pid }),
  node_controller(Topology);
node_receive(Topology, { you_are_alive, Id, Nb }) ->
  node_controller(#topology{
    neighbours = Topology#topology.neighbours,
    nb_nodes = Nb,
    id = Id
  });
node_receive(Topology, { he_is_alive, Id, Pid }) ->
  io:fwrite("~w: ~w ~w~n", [self(), Id, Pid]),
  node_controller(#topology{
    neighbours = case pop_count(Id bxor Topology#topology.id) of
       1 -> node_send({add_neighbour, {Topology#topology.id, self()}}, Id),
            [{Id, Pid}|Topology#topology.neighbours];
       _ -> Topology#topology.neighbours
    end,
    nb_nodes = Topology#topology.nb_nodes + 1,
    id = Topology#topology.id
  });
node_receive(Topology, { add_neighbour, Neighbour }) ->
  node_controller(Topology#topology{
    neighbours = [Neighbour|Topology#topology.neighbours]
  });
node_receive(Topology, Data) ->
  io:fwrite("~w(~w) -> ~w~n", [Topology#topology.id, self(), Data]),
  node_controller(Topology).

% === Node communications === %
node_send(Data, To) ->
  self() ! { send, To, Data }.

node_broadcast(Data) ->
  self() ! { broadcast, unknown, Data }.

% ===== Node controller ===== %
node_controller(Topology) ->
  receive
    { broadcast, unknown, Data } ->
      self() ! { broadcast, Topology#topology.id, Data },
      node_controller(Topology);
    { broadcast, From, Data } ->
      io:fwrite("broadcast: ~w ~w ~w~n", [self(), From, Data]),
      Diff = From bxor Topology#topology.id,
      Send = fun({Id,Pid})->
        Next = Id bxor Topology#topology.id,
        case Next > Diff of
          true -> io:fwrite("forwarding to ~w~n", [Id]),Pid ! { broadcast, From, Data };
          false -> ok
        end
      end,
      lists:map(Send, Topology#topology.neighbours),
      node_receive(Topology, Data);
    { send, unknown, Data } ->
      node_receive(Topology, Data);
    { send, To, Data } ->
      Diff = To bxor Topology#topology.id,
      case Diff band -Diff of
        0 -> node_receive(Topology, Data);
        X -> NextId = Topology#topology.id bxor X,
             Next = lists:keyfind(NextId, 1, Topology#topology.neighbours),
             element(2, Next) ! { send, To, Data },
             node_controller(Topology)
      end
  end.

% ======== Init node ======== %
node_init() ->
  receive Topology ->
    %io:fwrite("init : ~w -> ~w~n", [self(), Topology]),
    node_controller(Topology)
  end.

node_join(undefined) ->
  Topology = #topology{ id = 0, nb_nodes = 1, neighbours = [] },
  node_controller(Topology);
node_join(Pid) ->
  Pid ! { send, unknown, { i_am_alive, self() } },
  receive { Id, Nb } ->
    Topology = #topology{ id = Id, nb_nodes = Nb, neighbours = [] },
    node_controller(Topology)
  end.


% ====== Create network ===== %
network_init(Id, Id, _) -> ok;
network_init(Id, Nb, Pids) ->
  Neighbours = lists:map(fun(I) -> {I, lists:nth(I+1, Pids)} end,
                         get_neighbours(Id, Nb)),
  Topology = #topology{ id = Id, nb_nodes = Nb, neighbours = Neighbours },
  lists:nth(Id+1, Pids) ! Topology,
  network_init(Id+1, Nb, Pids).

network_create(N) ->
  Pids = spawns(N, fun node_init/0),
  network_init(0, N, Pids),
  Pids.

% ========== Test ========== %
test_network(Nb) ->
  Pids = network_create(Nb),
  lists:nth(3, Pids) ! { broadcast, unknown, print_yourself }.
test_join() ->
  Pid1 = spawn(agent, node_join, [undefined]),
  sleep(100),
  Pid1 ! { broadcast, unknown, print_yourself },
  sleep(100),
  Pid2 = spawn(agent, node_join, [Pid1]),
  sleep(100),
  Pid1 ! { broadcast, unknown, print_yourself },
  sleep(100),
  Pid3 = spawn(agent, node_join, [Pid1]),
  sleep(100),
  Pid1 ! { broadcast, unknown, print_yourself },
  sleep(100),
  Pid4 = spawn(agent, node_join, [Pid3]),
  sleep(100),
  Pid3 ! { broadcast, unknown, print_yourself },
  { Pid1, Pid2, Pid3, Pid4 }.
