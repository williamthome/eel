%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl converter module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_converter).

%% API functions
-export([to_string/1]).

%%======================================================================
%% API functions
%%======================================================================

%% -----------------------------------------------------------------------------
%% @doc to_string/1.
%% @end
%% -----------------------------------------------------------------------------
-spec to_string(term()) -> iodata().

to_string(List) when is_list(List) ->
    List;
to_string(Bin) when is_binary(Bin) ->
    Bin;
to_string(Fun) when is_function(Fun, 0) ->
    to_string(Fun());
to_string(undefined) ->
    <<>>;
to_string(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom);
% to_string(Float) when is_float(Float) ->
%     erlang:float_to_binary(Float);
to_string(Int) when is_integer(Int) ->
    erlang:integer_to_binary(Int);
to_string(PID) when is_pid(PID) ->
    erlang:pid_to_list(PID);
to_string(Term) ->
    io_lib:format("~p", [Term]).
