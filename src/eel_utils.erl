%%%-----------------------------------------------------------------------------
%%% @doc Embedded Erlang library utils.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_utils).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    term_to_string/1,
    to_string/1, to_string/2,
    group_by/2
]).

-spec term_to_string(term()) -> string().

term_to_string(Term) -> io_lib:format("~p", [Term]).

-spec to_string(term()) -> string().

to_string(Value) -> to_string(Value, #{}).

-spec to_string(term(), map()) -> string().

to_string(undefined, _Options) ->
    "";
to_string(Value, Options) when is_list(Value) ->
    try
        case io_lib:printable_list(Value) of
            true -> Value;
            false -> lists:concat([to_string(X, Options) || X <- Value])
        end
    catch
        _:_ -> term_to_string(Value)
    end;
to_string(Value, _Options) when is_integer(Value) ->
    try
        erlang:integer_to_list(Value)
    catch
        _:_ -> term_to_string(Value)
    end;
to_string(Value, _Options) when is_atom(Value) ->
    try
        erlang:atom_to_list(Value)
    catch
        _:_ -> term_to_string(Value)
    end;
to_string(Value, _Options) when is_binary(Value) ->
    try
        erlang:binary_to_list(Value)
    catch
        _:_ -> term_to_string(Value)
    end;
to_string(Value, Options) when is_float(Value) ->
    try
        erlang:float_to_list(Value, float_options(Options))
    catch
        _:_ -> term_to_string(Value)
    end;
to_string(Value, Options) when is_tuple(Value) ->
    try
        to_string(erlang:tuple_to_list(Value), Options)
    catch
        _:_ -> term_to_string(Value)
    end;
to_string(Value, _Options) when is_bitstring(Value) ->
    try
        erlang:bitstring_to_list(Value)
    catch
        _:_ -> term_to_string(Value)
    end;
to_string(Value, _Options) when is_pid(Value) ->
    try
        erlang:pid_to_list(Value)
    catch
        _:_ -> term_to_string(Value)
    end;
to_string(Value, _Options) when is_port(Value) ->
    try
        erlang:port_to_list(Value)
    catch
        _:_ -> term_to_string(Value)
    end;
to_string(Value, _Options) ->
    term_to_string(Value).

-spec group_by(fun((Elem :: T) -> Key), [T]) -> #{Key => [T]}.

group_by(Fun, List) when is_function(Fun, 1) ->
    Reversed = lists:reverse(List),
    do_group_by(Fun, Reversed, #{}).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec float_options(map()) -> proplist:proplist().

float_options(#{decimals := {Precision, compact}}) ->
    [{decimals, Precision}, compact];
float_options(#{decimals := Precision}) ->
    [{decimals, Precision}];
float_options(#{scientific := Precision}) ->
    [{scientific, Precision}];
float_options(#{}) ->
    [].

-spec do_group_by(fun((Elem :: T) -> Key), [T], #{Key => [T]}) -> #{Key => [T]}.

do_group_by(Fun, [Elem | Tail], AccIn) ->
    Key = Fun(Elem),
    AccOut =
        case AccIn of
            #{Key := Elems} -> AccIn#{Key := [Elem | Elems]};
            #{} -> AccIn#{Key => [Elem]}
        end,
    do_group_by(Fun, Tail, AccOut);
do_group_by(_Fun, [], Acc) ->
    Acc.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

to_string_test_() ->
    [
        {
            "Ensure string be unchanged",
            ?_assertEqual("foo", to_string("foo"))
        },
        {
            "Ensure convert list of terms to string",
            ?_assertEqual("foo", to_string([f, "o", <<"o">>]))
        },
        {
            "Ensure convert atom to string",
            ?_assertEqual("foo", to_string(foo))
        },
        {
            "Ensure convert binary to string",
            ?_assertEqual("foo", to_string(<<"foo">>))
        },
        {
            "Ensure convert integer to string",
            ?_assertEqual("1", to_string(1))
        },
        {
            "Ensure convert float to string",
            ?_assertEqual("1.10", to_string(1.10, #{decimals => 2}))
        },
        {
            "Ensure convert tuple of terms to string",
            ?_assertEqual("foo", to_string({f, "o", <<"o">>}))
        }
    ].

group_by_test() ->
    ?assertEqual(
        #{
            0 => [2],
            1 => [1, 3]
        },
        group_by(
            fun(X) -> X rem 2 end,
            [1, 2, 3]
        )
    ).

-endif.
