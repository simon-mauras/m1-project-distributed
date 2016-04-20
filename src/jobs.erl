-module(jobs).
-export([wait/1, mine/2]).

wait(Time) ->
  receive after Time -> ok end.
