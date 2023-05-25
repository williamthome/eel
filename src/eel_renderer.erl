%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl renderer module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_renderer).

%% API functions
-export([render/1, render/2, render/3, snapshot/3, snapshot/4, snapshot/5]).

%% Types
-export_type([bindings/0, snapshot/0, result/0]).

%% Includes
-include("eel_core.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Type
-type bindings() :: map().
-type snapshot() :: #{static   => eel_engine:static(),
                      dynamic  => [binary()],
                      ast      => eel_engine:ast(),
                      bindings => bindings(),
                      vars     => [atom()]}.
-type options()  :: map().
-type result()   :: {ok, snapshot()}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc render/1.
%% @end
%% -----------------------------------------------------------------------------
-spec render(Snapshot) -> Result when
    Snapshot :: snapshot(),
    Result   :: result().

render(Snapshot) ->
    render(#{}, Snapshot, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc render/2.
%% @end
%% -----------------------------------------------------------------------------
-spec render(Bindings, Snapshot) -> Result when
    Bindings :: bindings(),
    Snapshot :: snapshot(),
    Result   :: result().

render(Bindings, Snapshot) ->
    render(Bindings, Snapshot, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc render/3.
%% @end
%% -----------------------------------------------------------------------------
-spec render(Bindings, Snapshot, Opts) -> Result when
    Bindings :: bindings(),
    Snapshot :: snapshot(),
    Opts     :: options(),
    Result   :: result().

render(Changes0, #{static   := Static,
                   dynamic  := DynamicSnap,
                   ast      := AST,
                   bindings := BindingsSnap,
                   vars     := Vars}, Opts) ->
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
    {ok, snapshot(Static, Dynamic, AST, Bindings, Vars)}.

%% -----------------------------------------------------------------------------
%% @doc snapshot/3.
%% @end
%% -----------------------------------------------------------------------------
-spec snapshot(Static, Dynamic, AST) -> Result when
    Static   :: eel_engine:static(),
    Dynamic  :: [binary()],
    AST      :: eel_engine:ast(),
    Result   :: snapshot().

snapshot(Static, Dynamic, AST) ->
    snapshot(Static, Dynamic, AST, #{}).

%% -----------------------------------------------------------------------------
%% @doc snapshot/4.
%% @end
%% -----------------------------------------------------------------------------
-spec snapshot(Static, Dynamic, AST, Bindings) -> Result when
    Static   :: eel_engine:static(),
    Dynamic  :: [binary()],
    AST      :: eel_engine:ast(),
    Bindings :: bindings(),
    Result   :: snapshot().

snapshot(Static, Dynamic, AST, Bindings) ->
    Vars = eel_compiler:ast_vars(AST),
    snapshot(Static, Dynamic, AST, Bindings, Vars).

%% -----------------------------------------------------------------------------
%% @doc snapshot/5.
%% @end
%% -----------------------------------------------------------------------------
-spec snapshot(Static, Dynamic, AST, Bindings, Vars) -> Result when
    Static   :: eel_engine:static(),
    Dynamic  :: [binary()],
    AST      :: eel_engine:ast(),
    Bindings :: bindings(),
    Vars     :: [atom()],
    Result   :: snapshot().

snapshot(Static, Dynamic, AST, Bindings, Vars) ->
    #{static => Static,
      dynamic => Dynamic,
      ast => AST,
      bindings => Bindings,
      vars => Vars}.

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

capitalize_keys(Bindings, Opts) when is_list(Bindings) ->
    lists:map(fun({K, V}) -> {capitalize(K, Opts), V} end, Bindings);
capitalize_keys(Bindings, Opts) when is_map(Bindings) ->
    maps:fold(fun(K, V, Acc) -> Acc#{capitalize(K, Opts) => V} end, #{}, Bindings).

%% -----------------------------------------------------------------------------
%% @private
%% @doc Transform binary, list or atom to capital case. Snake case or camel case
%%      can be provided to be parsed, e.g.:
%%
%%          - snake_case: foo_bar, &lt;&lt;"foo_bar"&gt;&gt;, "foo_bar"
%%          - camelCase: fooBar, &lt;&lt;"fooBar"&gt;&gt;, "fooBar"
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
