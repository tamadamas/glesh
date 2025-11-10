-module(gleave).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleave.gleam").
-export([exit/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/gleave.gleam", 9).
?DOC(
    " Exit the program with the given status code\n"
    "\n"
    " ## Examples\n"
    " ```gleam\n"
    " gleave.exit(1) // exits with status code 1\n"
    " ```\n"
).
-spec exit(integer()) -> nil.
exit(Status) ->
    erlang:halt(Status).
