%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023-2024 William Fank Thomé
%% @doc Converter.

%% Copyright 2023-2024 William Fank Thomé
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(eel_converter).

%% API
-export([ to_string/1 ]).

%%%=====================================================================
%%% API
%%%=====================================================================

%%----------------------------------------------------------------------
%% @doc to_string/1.
%% @end
%%----------------------------------------------------------------------
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
