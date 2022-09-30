%%%-------------------------------------------------------------------
%% @doc eel public API
%% @end
%%%-------------------------------------------------------------------

-module(eel_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    eel_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
