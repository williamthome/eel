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
-type static()   :: eel_snapshot:static().
-type dynamic()  :: eel_engine:dynamic().
-type token()    :: eel_tokenizer:token().

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
    Static = eel_snapshot:get_static(Snapshot),
    Dynamic = eel_snapshot:get_dynamic(Snapshot),
    eval(Static, Dynamic).

%% -----------------------------------------------------------------------------
%% @doc eval/2.
%% @end
%% -----------------------------------------------------------------------------
-spec eval(Static, Dynamic) -> Result
    when Static  :: static()
       , Dynamic :: dynamic()
       , Result  :: iolist()
       .

eval(Static, Dynamic) ->
    retrieve_bin(zip(Static, Dynamic)).

%% -----------------------------------------------------------------------------
%% @doc retrieve_bin/1.
%% @end
%% -----------------------------------------------------------------------------
-spec retrieve_bin([token()]) -> iolist().

retrieve_bin(Tokens) ->
    lists:map(
        fun
            ({_, {_, IoData}}) when is_list(IoData); is_binary(IoData) ->
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
-spec zip({Static, Dynamic}) -> Result
    when Static  :: static()
       , Dynamic :: dynamic()
       , Result  :: [token()]
       .

zip({Static, Dynamic}) ->
    zip(Static, Dynamic).

%% -----------------------------------------------------------------------------
%% @doc zip/2.
%% @end
%% -----------------------------------------------------------------------------
-spec zip(static(), dynamic()) -> [token()].

zip(Static, Dynamic) ->
    do_zip(Static, Dynamic, []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_zip([S | Static], [D | Dynamic], Acc) ->
    do_zip(Static, Dynamic, [D, S | Acc]);
do_zip([S | Static], [], Acc) ->
    do_zip(Static, [], [S | Acc]);
do_zip([], [], Acc) ->
    lists:reverse(Acc);
do_zip([], Dynamic, Acc) ->
    lists:reverse(Dynamic, Acc).
