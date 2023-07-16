%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl evaluator module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_evaluator).

-compile(inline_list_funcs).
-compile({inline, [ do_zip/3 ]}).

%% API functions
-export([ eval/1
        , eval/2
        , zip/1
        , zip/2
        ]).

% Types
-type snapshot() :: eel_snapshot:snapshot().
-type statics()  :: eel_snapshot:statics().
-type dynamics() :: eel_engine:dynamics().
-type token()    :: eel_engine:token().

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc eval/1.
%% @end
%% -----------------------------------------------------------------------------
-spec eval(Snapshot) -> Result
    when Snapshot :: snapshot()
       , Result   :: iolist()
       .

eval(Snapshot) ->
    Statics = eel_snapshot:get_statics(Snapshot),
    Dynamics = eel_snapshot:get_dynamics(Snapshot),
    eval(Statics, Dynamics).

%% -----------------------------------------------------------------------------
%% @doc eval/2.
%% @end
%% -----------------------------------------------------------------------------
-spec eval(Statics, Dynamics) -> Result
    when Statics  :: statics()
       , Dynamics :: dynamics()
       , Result   :: iolist()
       .

eval(Statics, Dynamics) ->
    retrieve_bin(zip(Statics, Dynamics)).

%% -----------------------------------------------------------------------------
%% @doc retrieve_bin/1.
%% @end
%% -----------------------------------------------------------------------------
-spec retrieve_bin([token()]) -> iolist().

retrieve_bin(Tokens) ->
    lists:map(
        fun
            ({_, {_, _, IoData}}) when is_list(IoData); is_binary(IoData) ->
                IoData;
            (Term) ->
                eel_converter:to_string(Term)
        end,
        Tokens
    ).

%% -----------------------------------------------------------------------------
%% @doc zip/1.
%% @end
%% -----------------------------------------------------------------------------
-spec zip({Statics, Dynamics}) -> Result
    when Statics  :: statics()
       , Dynamics :: dynamics()
       , Result   :: [token()]
       .

zip({Statics, Dynamics}) ->
    zip(Statics, Dynamics).

%% -----------------------------------------------------------------------------
%% @doc zip/2.
%% @end
%% -----------------------------------------------------------------------------
-spec zip(statics(), dynamics()) -> [token()].

zip(Statics, Dynamics) ->
    do_zip(Statics, Dynamics, []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_zip([S | Statics], [D | Dynamics], Acc) ->
    do_zip(Statics, Dynamics, [D, S | Acc]);
do_zip([S | Statics], [], Acc) ->
    do_zip(Statics, [], [S | Acc]);
do_zip([], [], Acc) ->
    lists:reverse(Acc);
do_zip([], Dynamics, Acc) ->
    lists:reverse(Dynamics, Acc).
