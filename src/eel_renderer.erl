%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl renderer module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_renderer).

%% API functions
-export([render/1, render/2, render/3, zip/1, zip/2]).

%% Types
-export_type([result/0]).

%% Includes
-include("eel.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Types
-type result() :: tuple().

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc render/1.
%% @end
%% -----------------------------------------------------------------------------
-spec render(Tokens) -> Result when
    Tokens :: {eel_engine:static(), eel_engine:ast()},
    Result :: result().

render(Tokens) ->
    render(Tokens, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc render/2.
%% @end
%% -----------------------------------------------------------------------------
-spec render(TokensOrBindings, OptsOrTokens) -> Result when
    TokensOrBindings :: {eel_engine:static(), eel_engine:ast()} | map(),
    OptsOrTokens     :: map() | {eel_engine:static(), eel_engine:ast()},
    Result           :: result().

render(Tokens, Opts) when is_map(Opts) ->
    render(#{}, Tokens, Opts);
render(Bindings, Tokens) ->
    render(Bindings, Tokens, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc render/3.
%% @end
%% -----------------------------------------------------------------------------
-spec render(Bindings, Tokens, Opts) -> Result when
    Bindings :: map(),
    Tokens   :: {eel_engine:static(), eel_engine:ast()},
    Opts     :: map(),
    Result   :: result().

render(Bindings, {Static, AST}, Opts) ->
    Vars = eel_compiler:ast_vars(AST),
    render(Bindings, {Static, AST, Vars}, Opts);
render(Bindings0, {Static, AST, Vars}, Opts) ->
    Bindings1 = normalize_bindings(Bindings0, Opts),
    Bindings = erl_eval:add_binding('Bindings', Bindings1, Bindings1),
    Dynamic = lists:map(fun(Exprs) -> eval(Exprs, Bindings) end, AST),
    render_result(Static, AST, Vars, Dynamic, Bindings);
render(Changes0, {Static, AST, Vars, DynamicSnap, BindingsSnap}, Opts) ->
    Changes = normalize_bindings(Changes0, Opts),
    ChangesKeys = maps:keys(Changes),
    Bindings = maps:merge(BindingsSnap, Changes),
    Dynamic =
        lists:map(
            fun({Index, IndexVars}) ->
                case lists:any(fun(V) -> lists:member(V, ChangesKeys) end, IndexVars) of
                    true ->
                        Exprs = lists:nth(Index, AST),
                        eval(Exprs, Bindings);
                    false ->
                        lists:nth(Index, DynamicSnap)
                end
            end,
            Vars
        ),
    render_result(Static, AST, Vars, Dynamic, Bindings).

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
-spec zip(eel_engine:static(), eel_engine:dynamic()) -> list().

zip(Static, Dynamic) ->
    do_zip(Static, Dynamic, []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @private
%% @doc Normalize bindings keys to capitalized key, e.g.:
%%
%%          #{foo_bar => baz} -> #{'FooBar' => baz}
%%
%%      This is for the eval/2, who expects capitalized atoms.
%% @end
%% -----------------------------------------------------------------------------

normalize_bindings(Bindings0, #{}) ->
    capitalize_keys(Bindings0).

eval(Exprs, Bindings) ->
    {value, Binary, _} = erl_eval:exprs(Exprs, Bindings),
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

%% -----------------------------------------------------------------------------
%% @private
%% @doc Transform binary, list or atom to capital case. Snake case or camel case
%%      can be provided to be parsed, e.g.:
%%
%%          - snake_case: foo_bar, <<"foo_bar">>, "foo_bar"
%%          - camelCase: fooBar, <<"fooBar">>, "fooBar"
%%
%%      The result will be an atom: 'FooBar'.
%%      NOTE: The atom must exists, otherwise an exception will be raised.
%% @end
%% -----------------------------------------------------------------------------

capitalize(Key) ->
    capitalize_1(Key, Key).

capitalize_1(<<H, T/binary>>, _) when H >= $a, H =< $z ->
    capitalize_2(T, <<(H - 32)>>);
capitalize_1(<<H, _/binary>>, Key) when H >= $A, H =< $Z, is_atom(Key) ->
    Key;
capitalize_1(<<H, _/binary>> = Key, _) when H >= $A, H =< $Z ->
    binary_to_existing_atom(Key);
capitalize_1(Atom, Key) when is_atom(Atom) ->
    capitalize_1(atom_to_binary(Atom), Key);
capitalize_1(List, Key) when is_list(List) ->
    capitalize_1(list_to_binary(List), Key).

capitalize_2(<<$_, H, T/binary>>, Acc) when H >= $a, H =< $z ->
    capitalize_2(T, <<Acc/binary, (H - 32)>>);
capitalize_2(<<H, T/binary>>, Acc) ->
    capitalize_2(T, <<Acc/binary, H>>);
capitalize_2(<<>>, Acc) ->
    binary_to_existing_atom(Acc).

render_result(Static, AST, Vars, Dynamic, Bindings) ->
    Render = unicode:characters_to_nfc_binary(zip(Static, Dynamic)),
    Snapshot = {Static, AST, Vars, Dynamic, Bindings},
    {Render, Snapshot}.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

capitalize_test() ->
    [?assertEqual('FooBar', capitalize(<<"foo_bar">>)),
     ?assertEqual('FooBar', capitalize("foo_bar")),
     ?assertEqual('FooBar', capitalize(foo_bar)),
     ?assertEqual('FooBar', capitalize(fooBar)),
     ?assertEqual('FooBar', capitalize('FooBar'))].

capitalize_keys_test() ->
    ?assertEqual([{'FooBar', baz}], capitalize_keys([{<<"foo_bar">>, baz}])).

-endif.
