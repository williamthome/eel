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
-include("eel_core.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Types
-type snapshot() :: #{static   => eel_engine:static(),
                      ast      => eel_engine:ast(),
                      vars     => map(),
                      dynamic  => [binary()],
                      bindings => map()}.
-type result()   :: {ok, {binary(), snapshot()}}.

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

render(#{static := _, ast := _} = Tokens, Opts) ->
    render(#{}, Tokens, Opts);
render(Bindings, #{static := _, ast := _} = Tokens) ->
    render(Bindings, Tokens, ?DEFAULT_ENGINE_OPTS);
render(Tokens, Opts) when is_map(Opts) ->
    render(#{}, Tokens, Opts);
render(Bindings, {Static, AST}) ->
    render(Bindings, {Static, AST}, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc render/3.
%% @end
%% -----------------------------------------------------------------------------
-spec render(Bindings, Tokens, Opts) -> Result when
    Bindings :: map(),
    Tokens   :: {eel_engine:static(), eel_engine:ast()},
    Opts     :: map(),
    Result   :: result().

render(Changes0, #{static   := Static,
                   ast      := AST,
                   vars     := Vars,
                   dynamic  := DynamicSnap,
                   bindings := BindingsSnap}, Opts) ->
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
    render_result(Static, AST, Vars, Dynamic, Bindings);
render(Bindings0, #{static := Static,
                    ast    := AST,
                    vars   := Vars}, Opts) ->
    Bindings1 = normalize_bindings(Bindings0, Opts),
    Bindings = erl_eval:add_binding('Bindings', Bindings1, Bindings1),
    Dynamic = lists:map(fun(Exprs) -> eval(Exprs, Bindings) end, AST),
    render_result(Static, AST, Vars, Dynamic, Bindings);
render(Bindings, #{static := _, ast := AST} = Snapshot, Opts) ->
    Vars = eel_compiler:ast_vars(AST),
    render(Bindings, Snapshot#{vars => Vars}, Opts);
render(Bindings, {Static, AST}, Opts) ->
    Snapshot = #{static => Static,
                 ast    => AST,
                 vars   =>  eel_compiler:ast_vars(AST)},
    render(Bindings, Snapshot, Opts).

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

normalize_bindings(Bindings0, Opts) ->
    capitalize_keys(Bindings0, Opts).

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

capitalize_keys(Bindings, Opts) when is_list(Bindings) ->
    lists:map(fun({K, V}) -> {capitalize(K, Opts), V} end, Bindings);
capitalize_keys(Bindings, Opts) when is_map(Bindings) ->
    maps:fold(fun(K, V, Acc) -> Acc#{capitalize(K, Opts) => V} end, #{}, Bindings).

%% -----------------------------------------------------------------------------
%% @private
%% @doc Transform binary, list or atom to capital case. Snake case or camel case
%%      can be provided to be parsed, e.g.:
%%
%%          - snake_case: foo_bar, <<"foo_bar">>, "foo_bar"
%%          - camelCase: fooBar, <<"fooBar">>, "fooBar"
%%
%%      The result will be an atom: 'FooBar'.
%% @end
%% -----------------------------------------------------------------------------

capitalize(Key, Opts) ->
    capitalize_1(Key, Key, Opts).

capitalize_1(<<H, T/binary>>, _, Opts) when H >= $a, H =< $z ->
    capitalize_2(T, Opts, <<(H - 32)>>);
capitalize_1(<<H, _/binary>>, Key, _) when H >= $A, H =< $Z, is_atom(Key) ->
    Key;
capitalize_1(<<H, _/binary>> = Key, _, Opts) when H >= $A, H =< $Z ->
    to_atom(Key, Opts);
capitalize_1(Atom, Key, Opts) when is_atom(Atom) ->
    capitalize_1(erlang:atom_to_binary(Atom), Key, Opts);
capitalize_1(List, Key, Opts) when is_list(List) ->
    capitalize_1(erlang:list_to_binary(List), Key, Opts).
 
capitalize_2(<<$_, H, T/binary>>, Opts, Acc) when H >= $a, H =< $z ->
    capitalize_2(T, Opts, <<Acc/binary, (H - 32)>>);
capitalize_2(<<H, T/binary>>, Opts, Acc) ->
    capitalize_2(T, Opts, <<Acc/binary, H>>);
capitalize_2(<<>>, Opts, Acc) ->
    to_atom(Acc, Opts).

to_atom(Bin, #{safe_atoms := true}) ->
    erlang:binary_to_existing_atom(Bin);
to_atom(Bin, #{}) ->
    erlang:binary_to_atom(Bin).

render_result(Static, AST, Vars, Dynamic, Bindings) ->
    Render = unicode:characters_to_binary(zip(Static, Dynamic)),
    Snapshot = #{
        static => Static,
        ast => AST,
        vars => Vars,
        dynamic => Dynamic,
        bindings => Bindings
    },
    {ok, {Render, Snapshot}}.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

capitalize_test() ->
    [?assertEqual('FooBar', capitalize(<<"foo_bar">>, #{})),
     ?assertEqual('FooBar', capitalize("foo_bar", #{})),
     ?assertEqual('FooBar', capitalize(foo_bar, #{})),
     ?assertEqual('FooBar', capitalize(fooBar, #{})),
     ?assertEqual('FooBar', capitalize('FooBar', #{}))].

capitalize_keys_test() ->
    ?assertEqual([{'FooBar', baz}], capitalize_keys([{<<"foo_bar">>, baz}], #{})).

-endif.
