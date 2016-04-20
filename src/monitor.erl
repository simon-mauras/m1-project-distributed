-module(monitor).
-export([ask_merge/2, ask_split/2, ask_job/4,
         ask_root/1, ask_graph/2, ask_ping/1,
         test/1, start/0]).

% ================================ Utilities ================================= %

-define(WAITING_TIME, 1000).

spawns(0,_,_,_) -> [];
spawns(N, Mod, Fun, Args) ->
  [ spawn(Mod, Fun, Args) | spawns(N-1, Mod, Fun, Args) ].


% ================================ Monitoring ================================ %

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


% ================================== Test ==================================== %

test(N) ->
  % -------------------------------------------------------------------- %
  io:fwrite("Spawning agents..."),
  Pids = spawns(N, agent, node_init, [2]),
  io:fwrite(" Done!~n"),
  % -------------------------------------------------------------------- %
  io:fwrite("Merging..."),
  Pid1 = lists:foldl(fun ask_merge/2, undefined, Pids),
  io:fwrite(" Done!~n"),
  % -------------------------------------------------------------------- %
  io:fwrite("Export graph1.dot~n"),
  ask_graph(Pid1, "graph1.dot"),
  % -------------------------------------------------------------------- %
  io:fwrite("Spliting..."),
  { Pid2, Pid3 } = ask_split(Pid1, N div 2),
  io:fwrite(" Done!~n"),
  % -------------------------------------------------------------------- %
  io:fwrite("Export graph2-1.dot~n"),
  ask_graph(Pid2, "graph2-1.dot"),
  % -------------------------------------------------------------------- %
  io:fwrite("Export graph2-2.dot~n"),
  ask_graph(Pid3, "graph2-2.dot"),
  % -------------------------------------------------------------------- %
  io:fwrite("Merging..."),
  Pid = ask_merge(Pid2, Pid3),
  io:fwrite(" Done!~n"),
  % -------------------------------------------------------------------- %
  io:fwrite("Submitting jobs..."),
  Args = lists:map(fun (_) -> [1000] end, lists:seq(1, 10*N)),
  lists:map(fun (Arg) -> ask_job(Pid2, jobs, wait, Arg) end, Args),
  io:fwrite(" Done!~n"),
  % -------------------------------------------------------------------- %
  io:fwrite("Export graph3.dot~n"),
  ask_graph(Pid, "graph3.dot"),
  % -------------------------------------------------------------------- %
  io:fwrite("Waiting for the results..."),
  lists:map(fun (_) -> receive ok -> ok end end, Args),
  io:fwrite(" Done!~n"),
  % -------------------------------------------------------------------- %
  io:fwrite("Export graph4.dot~n"),
  ask_graph(Pid, "graph4.dot"),
  % -------------------------------------------------------------------- %
  io:fwrite("Killing agents..."),
  lists:map(fun (P) -> P ! { kill } end, Pids),
  io:fwrite(" Done!~n"),
  % -------------------------------------------------------------------- %
  ok.
  
start() ->
  net_adm:world(),
  Pids = lists:map(fun(X) -> ask_root({agent,X}) end, nodes()),
  Pid = lists:foldl(fun ask_merge/2, undefined, Pids),
  global:register_name(agent, Pid),
  ok.

stop() ->
  global:unregister_name(agent),
  lists:map(fun(X) -> {agent,X} ! {kill} end, nodes()),
  ok.

