%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl converter module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(v2_eel_converter).

%% API functions
-export([ to_string/1
        , to_string/2
        ]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc to_string/1.
%% @end
%% -----------------------------------------------------------------------------
-spec to_string(term()) -> iodata().

to_string(Value) ->
    to_string(Value, undefined).

%% -----------------------------------------------------------------------------
%% @doc to_string/2.
%% @end
%% -----------------------------------------------------------------------------
-spec to_string(term(), Options :: term()) -> iodata().

to_string(List, _) when is_list(List) ->
    List;
to_string(Bin, _) when is_binary(Bin) ->
    Bin;
to_string(Fun, _) when is_function(Fun, 0) ->
    to_string(Fun());
to_string(undefined, _) ->
    <<>>;
to_string(Atom, _) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom);
to_string(Atom, Encoding) when is_atom(Atom), is_atom(Encoding) ->
    erlang:atom_to_binary(Atom, Encoding);
to_string(Float, _) when is_float(Float) ->
    erlang:float_to_binary(Float);
to_string(Float, Options) when is_float(Float), is_list(Options) ->
    erlang:float_to_binary(Float, Options);
to_string(Int, _) when is_integer(Int) ->
    erlang:integer_to_binary(Int);
to_string(Int, Base) when is_integer(Int), is_integer(Base) ->
    erlang:integer_to_binary(Int, Base);
to_string(PID, _) when is_pid(PID) ->
    erlang:pid_to_list(PID);
to_string(Term, _) ->
    io_lib:format("~p", [Term]).
