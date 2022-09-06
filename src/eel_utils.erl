-module(eel_utils).

-export([to_binary/1, to_binary/2]).

-dialyzer({nowarn_function, to_binary/2}).

to_binary(Value) ->
    to_binary(Value, undefined).

to_binary(Bin, _) when is_binary(Bin) ->
    Bin;
to_binary(undefined, _) ->
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
    erlang:iolist_to_binary(List);
to_binary(Tuple, undefined) when is_tuple(Tuple) ->
    to_binary(erlang:tuple_to_list(Tuple));
to_binary(_, _) ->
    <<>>.
