%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl renderer module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_renderer).

-compile(inline_list_funcs).
-compile({inline, [ render/3
                  , capitalize_keys/2
                  ]}).

%% API functions
-export([ render/1
        , render/2
        , render/3
        ]).

%% Types
-export_type([ bindings/0
             , dynamic/0
             , changes/0
             , snapshot/0
             , result/0
             ]).

%% Includes
-include("eel_core.hrl").
-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Types
-type snapshot() :: eel_snapshot:snapshot().
-type bindings() :: eel_snapshot:bindings().
-type dynamic()  :: eel_snapshot:dynamice().
-type changes()  :: eel_snapshot:changes().
-type options()  :: map().
-type result()   :: {ok, snapshot()}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc render/1.
%% @end
%% -----------------------------------------------------------------------------
-spec render(Snapshot) -> Result
    when Snapshot :: snapshot()
       , Result   :: result()
       .

render(Snapshot) ->
    render(#{}, Snapshot, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc render/2.
%% @end
%% -----------------------------------------------------------------------------
-spec render(Bindings, Snapshot) -> Result
    when Bindings :: bindings()
       , Snapshot :: snapshot()
       , Result   :: result()
       .

render(Bindings, Snapshot) ->
    render(Bindings, Snapshot, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc render/3.
%% @end
%% -----------------------------------------------------------------------------
-spec render(Bindings, Snapshot, Opts) -> Result
    when Bindings :: bindings()
       , Snapshot :: snapshot()
       , Opts     :: options()
       , Result   :: result()
       .

render(Params0, Snapshot, Opts) ->
    Static = eel_snapshot:get_static(Snapshot),
    DynamicSnap = eel_snapshot:get_dynamic(Snapshot),
    AST = eel_snapshot:get_ast(Snapshot),
    BindingsSnap = eel_snapshot:get_bindings(Snapshot),
    Vars = eel_snapshot:get_vars(Snapshot),
    Params = normalize_bindings(Params0, Opts),
    Bindings = maps:merge(BindingsSnap, Params),
    EvalBindings = Bindings#{'Bindings' => Bindings},
    {Dynamic0, Changes0} =
        lists:foldl(
            fun({Index, IndexVars}, {DAcc, CAcc}) ->
                case should_eval_exprs(DynamicSnap, Params, IndexVars) of
                    true ->
                        {Index, {Pos, EvalAST}} = proplists:lookup(Index, AST),
                        try
                            Bin = eval(EvalAST, EvalBindings),
                            {[{Index, {Pos, Bin}} | DAcc], [{Index, Bin} | CAcc]}
                        catch
                            Class:Reason:Stacktrace ->
                                ?LOG_ERROR(#{ class => Class
                                            , reason => Reason
                                            , stacktrace => Stacktrace
                                            , module => ?MODULE
                                            , function => ?FUNCTION_NAME
                                            , arity => ?FUNCTION_ARITY
                                            , ast => EvalAST
                                            , position => Pos
                                            , bindings => Bindings
                                            }),
                                erlang:raise(Class, Reason, Stacktrace)
                        end;
                    false ->
                        DCache = proplists:lookup(Index, DynamicSnap),
                        {[DCache | DAcc], CAcc}
                end
            end,
            {[], []},
            Vars
        ),
    Dynamic = lists:reverse(Dynamic0),
    Changes = lists:reverse(Changes0),
    {ok, eel_snapshot:new(Static, Dynamic, AST, Bindings, Vars, Changes)}.

should_eval_exprs(undefined, _, _) ->
    true;
should_eval_exprs(_, Params, Vars) ->
    lists:member('Bindings', Vars) orelse contains_any_var(Params, Vars).

eval(Exprs, Bindings) ->
    {value, Binary, _} = erl_eval:exprs(Exprs, Bindings),
    Binary.

contains_any_var(Map, Vars) ->
    MapKeys = maps:keys(Map),
    lists:any(fun(V) -> lists:member(V, MapKeys) end, Vars).

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

normalize_bindings(Bindings, #{snake_case := true} = Opts) ->
    capitalize_keys(Bindings, Opts);
normalize_bindings(Bindings, #{}) ->
    Bindings.

capitalize_keys(Bindings, Opts) when is_list(Bindings) ->
    lists:map(fun({K, V}) -> {capitalize(K, Opts), V} end, Bindings);
capitalize_keys(Bindings, Opts) when is_map(Bindings) ->
    maps:fold(fun(K, V, Acc) -> Acc#{capitalize(K, Opts) => V} end, #{}, Bindings).

%% -----------------------------------------------------------------------------
%% @private
%% @doc Transforms a snake_case atom() to CameCase, e.g.:
%%
%%          foo_bar: &lt;&lt;"FooBar"&gt;&gt
%% @end
%% -----------------------------------------------------------------------------

capitalize(Atom, Opts) when is_atom(Atom) ->
    to_atom(do_capitalize(erlang:atom_to_binary(Atom)), Opts).

do_capitalize(<<H, T/binary>>) when H >= $a, H =< $z ->
    do_capitalize_1(T, <<(H - 32)>>);
do_capitalize(Bin) when is_binary(Bin) ->
    do_capitalize_1(Bin, <<>>).

do_capitalize_1(<<$_, H, T/binary>>, Acc) when H >= $a, H =< $z ->
    do_capitalize_1(T, <<Acc/binary, (H - 32)>>);
do_capitalize_1(<<H, T/binary>>, Acc) ->
    do_capitalize_1(T, <<Acc/binary, H>>);
do_capitalize_1(<<>>, Acc) ->
    Acc.

to_atom(Bin, #{safe_atoms := true}) ->
    erlang:binary_to_existing_atom(Bin);
to_atom(Bin, #{}) ->
    erlang:binary_to_atom(Bin).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

capitalize_test() ->
    ?assertEqual('FooBar', capitalize(foo_bar, #{})).

capitalize_keys_test() ->
    [ ?assertEqual([{'FooBar', baz}], capitalize_keys([{foo_bar, baz}], #{}))
    , ?assertEqual(#{'FooBar' => baz}, capitalize_keys(#{foo_bar => baz}, #{}))
    ].

-endif.
