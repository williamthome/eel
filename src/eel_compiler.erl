%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl compiler module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_compiler).

%% API functions
-export([compile/1, compile/2,
         compile_to_module/2, compile_to_module/3,
         bin_ast_form/1, expr_to_ast/1,
         normalize_expr/1,
         merge_sd/1, merge_sd/2]).

%% Includes
-include("eel.hrl").
-include_lib("syntax_tools/include/merl.hrl").

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc compile/1.
%% @end
%% -----------------------------------------------------------------------------
-spec compile(eel_tokenizer:tokens()) -> list().

compile(Tokens) ->
    compile(Tokens, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc compile/2.
%% @end
%% -----------------------------------------------------------------------------
-spec compile(eel_tokenizer:tokens(), map()) -> list().

compile({Static, Dynamic}, Opts) when is_list(Static), is_list(Dynamic) ->
    Eng = maps:get(engine, Opts, ?DEFAULT_ENGINE),
    State = Eng:init(Opts),
    DAST = do_compile(Dynamic, Eng, State),
    SAST = lists:map(fun bin_ast_form/1, Static),
    merge_sd(SAST, DAST).

%% -----------------------------------------------------------------------------
%% @doc compile_to_module/2.
%% @end
%% -----------------------------------------------------------------------------
-spec compile_to_module(file:filename_all(), eel_tokenizer:tokens()) ->
    {ok, module()} | {error, term()}.

compile_to_module(FileOrModName, Tokens) ->
    compile_to_module(FileOrModName, Tokens, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc compile_to_module/3.
%% @end
%% -----------------------------------------------------------------------------
-spec compile_to_module(file:filename_all(), eel_tokenizer:tokens(), map()) ->
    {ok, module()} | {error, term()}.

compile_to_module(FileOrModName, Tokens, Opts) ->
    Module = module_name(FileOrModName),
    case erlang:module_loaded(Module) of
        true ->
            {ok, Module};
        false ->
            AST = compile(Tokens, Opts),
            Forms = ?Q(["-module('@module').",
                        "",
                        "-export([filename/0,",
                        "         render/1,",
                        "         render/2]).",
                        "",
                        "filename() -> _@filename.",
                        "",
                        "render(Bindings) ->",
                        "    render(Bindings, #{}).",
                        "",
                        "render(Bindings, Opts0) ->",
                        "    Opts = maps:merge(_@compiler_options, Opts0),",
                        "    eel_renderer:render(_@ast, Bindings, Opts)."],
                        [{module, merl:term(Module)},
                         {filename, merl:term(
                            case is_binary(FileOrModName) of
                                true -> FileOrModName;
                                false -> undefined
                            end)},
                         {ast, merl:term(AST)},
                         {compiler_options, merl:term(Opts)}]),
            Forms1 = [erl_syntax:revert(Form) || Form <- Forms],
            {ok, Module, Bin} = compile:forms(Forms1),
            ModuleBin = atom_to_binary(Module),
            ModuleFileName = erlang:binary_to_list(<<ModuleBin/binary, ".erl">>),
            case code:load_binary(Module, ModuleFileName, Bin) of
                {module, Module} ->
                    {ok, Module};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% -----------------------------------------------------------------------------
%% @doc bin_ast_form/1.
%% @end
%% -----------------------------------------------------------------------------
-spec bin_ast_form(binary()) -> eel_engine:ast().

bin_ast_form(Bin) ->
    [{bin, 1, [{bin_element, 1, {string, 1, binary_to_list(Bin)}, default, default}]}].

%% -----------------------------------------------------------------------------
%% @doc expr_to_ast/1.
%% @end
%% -----------------------------------------------------------------------------
-spec expr_to_ast(binary() | string()) -> eel_engine:ast().

expr_to_ast(Expr) ->
    {ok, Tokens, _} = erl_scan:string(normalize_expr(Expr)),
    {ok, AST} = erl_parse:parse_exprs(Tokens),
    AST.

%% -----------------------------------------------------------------------------
%% @doc normalize_expr/1.
%% @end
%% -----------------------------------------------------------------------------
-spec normalize_expr(binary() | string()) -> string().

normalize_expr(Expr) ->
    erlang:binary_to_list(erlang:iolist_to_binary([Expr, "."])).

%% -----------------------------------------------------------------------------
%% @doc merge_sd/1.
%% @end
%% -----------------------------------------------------------------------------
-spec merge_sd(eel_tokenizer:tokens()) -> list().

merge_sd({Static, Dynamic}) ->
    merge_sd(Static, Dynamic).

%% -----------------------------------------------------------------------------
%% @doc merge_sd/2.
%% @end
%% -----------------------------------------------------------------------------
-spec merge_sd([binary()], list()) -> list().

merge_sd(Static, Dynamic) ->
    do_merge_sd(Static, Dynamic, []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_compile([D | Dynamic], Eng, State) ->
    NewState = Eng:handle_compile(D, State),
    do_compile(Dynamic, Eng, NewState);
do_compile([], Eng, State) ->
    Eng:handle_ast(State).

module_name(Filename) when is_binary(Filename); is_list(Filename) ->
    Basename =
        case filename:basename(Filename) of
            N when is_binary(N) -> N;
            N when is_list(N) -> list_to_binary(N)
        end,
    Name = binary:replace(Basename, <<".">>, <<"_">>, [global]),
    case catch erlang:binary_to_existing_atom(Name) of
        Atom when is_atom(Atom) -> Atom;
        _ -> erlang:binary_to_atom(Name)
    end;
module_name(Mod) when is_atom(Mod) ->
    Mod.

do_merge_sd([S | Static], [D | Dynamic], Acc) ->
    do_merge_sd(Static, Dynamic, [D, S | Acc]);
do_merge_sd([S | Static], [], Acc) ->
    do_merge_sd(Static, [], [S | Acc]);
do_merge_sd([], [], Acc) ->
    lists:reverse(Acc);
do_merge_sd([], Dynamic, Acc) ->
    lists:reverse(Dynamic, Acc).
