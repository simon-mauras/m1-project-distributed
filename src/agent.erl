-module(agent).
-export([network_create/1, test/2]).

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

% ===== Node receive ===== %
node_receive(Topology, Data) ->
  io:fwrite("~w(~w) -> ~w~n", [Topology#topology.id, self(), Data]),
  node_controller(Topology).

% === Node communications === %
node_send(To, Data) ->
  self() ! { send, To, Data }.

node_broadcast(Data) ->
  self() ! { broadcast, self(), Data }.

% ===== Node controller ===== %
node_controller(Topology) ->
  receive
    { broadcast, From, Data } ->
      Diff = From bxor Topology#topology.id,
      Send = fun({Id,Pid})->
        Next = Id bxor Topology#topology.id,
        case Next > Diff of
          true -> Pid ! { broadcast, From, Data };
          false -> ok
        end
      end,
      lists:map(Send, Topology#topology.neighbours),
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
    io:fwrite("init : ~w -> ~w~n", [self(), Topology]),
    node_controller(Topology)
  end.

% ====== Create network ===== %
network_init(Id, Id, _) -> ok;
network_init(Id, Nb, Pids) ->
  Neighbours = lists:map(fun(I) -> {I, lists:nth(I+1, Pids)} end,
                         get_neighbours(Id, Nb)),
  Topology = #topology{id = Id, nb_nodes = Nb, neighbours = Neighbours},
  lists:nth(Id+1, Pids) ! Topology,
  network_init(Id+1, Nb, Pids).

network_create(N) ->
  Pids = spawns(N, fun node_init/0),
  network_init(0, N, Pids),
  Pids.

% ========== Test ========== %
test(Id, Nb) ->
  Pids = network_create(Nb),
  lists:nth(Id+1,Pids) ! {broadcast, Id, broadcast},
  lists:nth(Id+1,Pids) ! {send, 0, send}.
