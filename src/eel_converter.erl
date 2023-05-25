%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl converter utils module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_converter).

%% API functions
-export([to_binary/1, to_binary/2]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc to_binary/1.
%% @end
%% -----------------------------------------------------------------------------
-spec to_binary(term()) -> binary().

to_binary(Value) ->
    to_binary(Value, undefined).

%% -----------------------------------------------------------------------------
%% @doc to_binary/2.
%% @end
%% -----------------------------------------------------------------------------
-spec to_binary(term(), Options :: term()) -> binary().

to_binary(Bin, undefined) when is_binary(Bin) ->
    Bin;
to_binary(Fun, undefined) when is_function(Fun, 0) ->
    to_binary(Fun());
to_binary(undefined, _) ->
    <<>>;
to_binary([], _) ->
    <<>>;
to_binary(Atom, undefined) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom);
to_binary(Atom, Encoding) when is_atom(Atom), is_atom(Encoding) ->
    erlang:atom_to_binary(Atom, Encoding);
to_binary(Float, undefined) when is_float(Float) ->
    erlang:float_to_binary(Float);
to_binary(Float, Options) when is_float(Float), is_list(Options) ->
    erlang:float_to_binary(Float, Options);
to_binary(Int, undefined) when is_integer(Int) ->
    erlang:integer_to_binary(Int);
to_binary(Int, Base) when is_integer(Int), is_integer(Base) ->
    erlang:integer_to_binary(Int, Base);
to_binary(List, undefined) when is_list(List) ->
    case io_lib:deep_unicode_char_list(List) of
        true ->
            erlang:list_to_binary(List);
        false ->
            lists:map(fun to_binary/1, List)
    end;
to_binary(Tuple, undefined) when is_tuple(Tuple) ->
    to_binary(erlang:tuple_to_list(Tuple));
to_binary(PID, undefined) when is_pid(PID) ->
    erlang:list_to_binary(erlang:pid_to_list(PID));
to_binary(Term, _) ->
    erlang:list_to_binary(io_lib:format("~p", [Term])).
