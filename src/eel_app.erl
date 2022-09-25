%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2022, williamthome
%%% @doc EEl public API.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome/]
%%% @since 2022
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_app).

-behaviour(application).

%% API functions
-export([
    start/2,
    stop/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start(_StartType, _StartArgs) ->
    eel_template_sup:start_link().

stop(_State) ->
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here yet!
