-module(jobs).
-export([wait/1]).

wait(Time) ->
  receive after Time -> ok end.
