-module(gleave).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([exit/1]).

-file("/home/nick/Documents/code/gleave/src/gleave.gleam", 9).
-spec exit(integer()) -> nil.
exit(Status) ->
    erlang:halt(Status).
