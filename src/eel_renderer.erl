%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl renderer module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_renderer).

%% API functions
-export([render/1, render/2, render/3, zip/1, zip/2]).

%% Includes
-include("eel.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc render/1.
%% @end
%% -----------------------------------------------------------------------------
-spec render(list()) -> binary().

render(AST) ->
    render(AST, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc render/2.
%% @end
%% -----------------------------------------------------------------------------
-spec render(list(), map() | proplists:proplist()) -> binary().

render(ASTList, Bindings) ->
    render(ASTList, Bindings, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc render/3.
%% @end
%% -----------------------------------------------------------------------------
-spec render(list(), map() | proplists:proplist(), map()) -> binary().

render({Static, DynamicAST}, Bindings, Opts) ->
    Dynamic = lists:map(fun(AST) -> eval(AST, Bindings, Opts) end, DynamicAST),
    unicode:characters_to_nfc_binary(zip(Static, Dynamic)).

%% -----------------------------------------------------------------------------
%% @doc zip/1.
%% @end
%% -----------------------------------------------------------------------------
-spec zip(eel_tokenizer:tokens()) -> list().

zip({Static, Dynamic}) ->
    zip(Static, Dynamic).

%% -----------------------------------------------------------------------------
%% @doc zip/2.
%% @end
%% -----------------------------------------------------------------------------
-spec zip([binary()], list()) -> list().

zip(Static, Dynamic) ->
    do_zip(Static, Dynamic, []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

eval(AST, Bindings0, Opts) ->
    Bindings1 =
        case maps:find(capitalize, Opts) of
            {ok, true} ->
                capitalize_keys(Bindings0);
            {ok, false} ->
                Bindings0;
            error ->
                Bindings0
        end,
    Bindings = erl_eval:add_binding('Bindings', Bindings1, Bindings1),
    {value, Binary, _} = erl_eval:exprs(AST, Bindings),
    Binary.

do_zip([S | Static], [D | Dynamic], Acc) ->
    do_zip(Static, Dynamic, [D, S | Acc]);
do_zip([S | Static], [], Acc) ->
    do_zip(Static, [], [S | Acc]);
do_zip([], [], Acc) ->
    lists:reverse(Acc);
do_zip([], Dynamic, Acc) ->
    lists:reverse(Dynamic, Acc).

capitalize_keys(Bindings) when is_list(Bindings) ->
    lists:map(fun({K, V}) -> {capitalize(K), V} end, Bindings);
capitalize_keys(Bindings) when is_map(Bindings) ->
    maps:fold(fun(K, V, Acc) -> Acc#{capitalize(K) => V} end, #{}, Bindings).

capitalize(<<H, T/binary>>) when H >= $a, H =< $z ->
    capitalize(T, <<(H - 32)>>);
capitalize(<<_, T/binary>>) ->
    capitalize(T);
capitalize(Atom) when is_atom(Atom) ->
    capitalize(atom_to_binary(Atom));
capitalize(List) when is_list(List) ->
    capitalize(list_to_binary(List)).

capitalize(<<$_, H, T/binary>>, Acc) when H >= $a, H =< $z ->
    capitalize(T, <<Acc/binary, (H - 32)>>);
capitalize(<<H, T/binary>>, Acc) ->
    capitalize(T, <<Acc/binary, H>>);
capitalize(<<>>, Acc) ->
    binary_to_existing_atom(Acc).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

capitalize_test() ->
    [?assertEqual('FooBar', capitalize(<<"foo_bar">>)),
     ?assertEqual('FooBar', capitalize("foo_bar")),
     ?assertEqual('FooBar', capitalize(foo_bar))].

capitalize_keys_test() ->
    [?assertEqual([{'FooBar', baz}], capitalize_keys([{<<"foo_bar">>, baz}])),
     ?assertEqual(#{'FooBar' => baz}, capitalize_keys(#{<<"foo_bar">> => baz}))].

-endif.
