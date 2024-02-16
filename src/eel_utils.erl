%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023-2024 William Fank Thomé
%% @doc Support functions.

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
-module(eel_utils).

%% API
-export([ replace_in_expr/3
        , expand_expr_macros/2
        , expand_expr_macro/3
        , mod_records/1
        , mod_macros/1
        ]).

%%%=====================================================================
%%% API
%%%=====================================================================

replace_in_expr(Pattern, Replacement0, Expr) ->
    Replacement = eel_converter:to_string(Replacement0),
    iolist_to_binary(string:replace(Expr, Pattern, Replacement, all)).

expand_expr_macros(Macros, Expr) ->
    maps:fold(fun expand_expr_macro/3, Expr, Macros).

% FIXME: Ignore when quoted (string).
expand_expr_macro(Name, Value, Expr) ->
    Pattern = <<"?", (atom_to_binary(Name))/binary>>,
    replace_in_expr(Pattern, Value, Expr).

% IMPORTANT!
% debug_info MUST be enabled in compile options,
% otherwise no abstract code would return.
% @see https://www.erlang.org/doc/man/compile.html#file-2
mod_records(Mod) ->
    Beam = code:which(Mod),
    {ok, {Mod, Chunks}} = beam_lib:chunks(Beam, [abstract_code]),
    [{abstract_code, {raw_abstract_v1, AST}}] = Chunks,
    [R || R = {attribute, _, record, _} <- AST].

mod_macros(Mod) ->
    {source, Src} = proplists:lookup(source, Mod:module_info(compile)),
    {ok, Bin} = file:read_file(Src),
    Pattern = <<"-define\\(([a-zA-Z_]*)\s*,\s*(.*?)\\)\s*\\.">>,
    Opts = [global, {capture, all_but_first, binary}],
    case re:run(Bin, Pattern, Opts) of
        {match, Macros0} ->
            Macros = lists:foldl(fun([Name, Value], Acc) ->
                Acc#{binary_to_atom(Name) => Value}
            end, #{}, Macros0),
            maps:map(fun(_Name, Value) ->
                expand_expr_macros(Macros, Value)
            end, Macros);
        nomatch ->
            #{}
    end.
