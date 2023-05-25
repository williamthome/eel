%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl compiler module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_evaluator).

%% API functions
-export([eval/1, eval/2, zip/1, zip/2]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @evall/1.
%% @end
%% -----------------------------------------------------------------------------
-spec eval(Snapshot) -> Result when
    Snapshot :: eel_renderer:snapshot(),
    Result   :: binary().

eval(#{static := Static, dynamic := Dynamic}) ->
    eval(Static, Dynamic).

%% -----------------------------------------------------------------------------
%% @evall/2.
%% @end
%% -----------------------------------------------------------------------------
-spec eval(Static, Dynamic) -> Result when
    Static  :: eel_engine:static(),
    Dynamic :: [binary()],
    Result  :: binary().

eval(Static, Dynamic) ->
    unicode:characters_to_binary(zip(Static, Dynamic)).

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

do_zip([S | Static], [D | Dynamic], Acc) ->
    do_zip(Static, Dynamic, [D, S | Acc]);
do_zip([S | Static], [], Acc) ->
    do_zip(Static, [], [S | Acc]);
do_zip([], [], Acc) ->
    lists:reverse(Acc);
do_zip([], Dynamic, Acc) ->
    lists:reverse(Dynamic, Acc).
