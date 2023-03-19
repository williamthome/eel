%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl compiler module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_evaluator).

%% API functions
-export([eval/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

eval({ok, {Data, _}}) ->
    Data;
eval(Err) ->
    error(Err).
