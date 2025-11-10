-module(main).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/main.gleam").
-export([main/0]).

-file("src/main.gleam", 59).
-spec print_not_found_message(binary()) -> nil.
print_not_found_message(Command) ->
    gleam_stdlib:println(<<Command/binary, ": command not found"/utf8>>).

-file("src/main.gleam", 29).
-spec handle_command(binary(), list(binary())) -> nil.
handle_command(Command, Args) ->
    case Command of
        <<"exit"/utf8>> ->
            case Args of
                [] ->
                    erlang:halt(0);

                _ ->
                    _pipe = Args,
                    _pipe@1 = gleam@list:first(_pipe),
                    _pipe@2 = gleam@result:'try'(
                        _pipe@1,
                        fun gleam_stdlib:parse_int/1
                    ),
                    _pipe@3 = gleam@result:unwrap(_pipe@2, 1),
                    erlang:halt(_pipe@3)
            end;

        <<"echo"/utf8>> ->
            gleam_stdlib:println(gleam@string:join(Args, <<" "/utf8>>));

        <<"type"/utf8>> ->
            Target = gleam@result:unwrap(gleam@list:first(Args), <<""/utf8>>),
            Found = gleam@list:find(
                [<<"exit"/utf8>>, <<"echo"/utf8>>, <<"type"/utf8>>],
                fun(X) -> X =:= Target end
            ),
            Message = case Found of
                {ok, _} ->
                    <<" is a shell builtin"/utf8>>;

                {error, _} ->
                    <<": not found"/utf8>>
            end,
            gleam_stdlib:println(<<Target/binary, Message/binary>>);

        _ ->
            print_not_found_message(Command)
    end.

-file("src/main.gleam", 17).
-spec run_loop() -> nil.
run_loop() ->
    User_input@1 = case input_ffi:input(<<"$ "/utf8>>) of
        {ok, User_input} -> User_input;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"main"/utf8>>,
                        function => <<"run_loop"/utf8>>,
                        line => 18,
                        value => _assert_fail,
                        start => 276,
                        'end' => 323,
                        pattern_start => 287,
                        pattern_end => 301})
    end,
    Parts = gleam@string:split(User_input@1, <<" "/utf8>>),
    case Parts of
        [] ->
            run_loop();

        [Command | Args] ->
            handle_command(Command, Args)
    end,
    run_loop().

-file("src/main.gleam", 13).
-spec main() -> nil.
main() ->
    run_loop().
