-module(server).
-export([start/0]).

run(Root, Pids) ->
  receive
    { new_node, New } ->
      io:fwrite("New node: ~w~n", [New]),
      run(monitor:ask_merge(Root, New), [New|Pids])
  end.

start() ->
  global:register_name(server, self()),global:sync(),
  run(undefined, []).
