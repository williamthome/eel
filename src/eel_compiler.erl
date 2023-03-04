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
         dynamic_to_ast/1, ast_vars/1]).

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
-spec compile(eel_engine:dynamic()) -> eel_engine:ast().

compile(Dynamic) ->
    compile(Dynamic, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc compile/2.
%% @end
%% -----------------------------------------------------------------------------
-spec compile(eel_engine:dynamic(), map()) -> eel_engine:ast().

compile(Dynamic, Opts) when is_list(Dynamic) ->
    Eng = maps:get(engine, Opts, ?DEFAULT_ENGINE),
    State = Eng:init(Opts),
    do_compile(Dynamic, Eng, State).

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
-spec compile_to_module(eel_tokenizer:tokens(), file:filename_all(), map()) ->
    {ok, module()} | {error, term()}.

compile_to_module({Static, Dynamic}, FileOrModName, Opts) ->
    Module = module_name(FileOrModName),
    case erlang:module_loaded(Module) of
        true ->
            {ok, Module};
        false ->
            AST = compile(Dynamic, Opts),
            Vars = ast_vars(AST),
            Forms = ?Q(["-module('@module').",
                        "",
                        "-export([filename/0,",
                        "         timestamp/0,",
                        "         static/0,",
                        "         ast/0,",
                        "         vars/0,",
                        "         compile_opts/0,",
                        "         render/1,",
                        "         render/2]).",
                        "",
                        "filename() -> _@filename.",
                        "",
                        "timestamp() -> _@timestamp.",
                        "",
                        "static() -> _@static.",
                        "",
                        "ast() -> _@ast.",
                        "",
                        "vars() -> _@vars.",
                        "",
                        "compile_opts() -> _@compile_opts.",
                        "",
                        "render(Bindings) ->",
                        "    render(Bindings, #{}).",
                        "",
                        "render(Bindings, Opts0) ->",
                        "    Opts = maps:merge(compile_opts(), Opts0),",
                        "    eel_renderer:render({static(), ast()}, Bindings, Opts).",
                        ""],
                        [{module, merl:term(Module)},
                         {filename, merl:term(
                            case is_binary(FileOrModName) of
                                true -> FileOrModName;
                                false -> undefined
                            end)},
                         {timestamp, merl:term(os:timestamp())},
                         {static, merl:term(Static)},
                         {ast, merl:term(AST)},
                         {vars, merl:term(Vars)},
                         {compile_opts, merl:term(Opts)}]),
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
%% @doc dynamic_to_ast/1.
%% @end
%% -----------------------------------------------------------------------------
-spec dynamic_to_ast(binary() | string()) -> eel_engine:ast().

dynamic_to_ast(Expr) ->
    {ok, Tokens, _} = erl_scan:string(normalize_expr(Expr)),
    {ok, AST} = erl_parse:parse_exprs(Tokens),
    AST.

%% -----------------------------------------------------------------------------
%% @doc ast_vars/1.
%% @end
%% -----------------------------------------------------------------------------
-spec ast_vars(eel_engine:ast()) -> [atom()].

ast_vars(AST) when is_list(AST) ->
    Opts = [
        nowarn_underscore_match,
        nowarn_export_all,
        nowarn_export_vars,
        nowarn_shadow_vars,
        nowarn_unused_import,
        nowarn_unused_function,
        nowarn_unused_type,
        nowarn_bif_clash,
        nowarn_unused_record,
        nowarn_deprecated_function,
        nowarn_deprecated_type,
        nowarn_obsolete_guard,
        nowarn_untyped_record,
        nowarn_missing_spec,
        nowarn_missing_spec_all,
        nowarn_removed,
        nowarn_nif_inline,
        nowarn_keywords
    ],
    case erl_lint:exprs_opt(lists:flatten(AST), [], Opts) of
        {ok, _Warns} ->
            [];
        {error, Errs, _Warns} ->
            lists:foldl(
                fun({"nofile", Errs1}, Acc) ->
                    lists:foldl(
                        fun
                            ({_Line, erl_lint, {unbound_var, Var}}, Acc1) ->
                                [Var | Acc1];
                            (_, Acc1) ->
                                Acc1
                        end,
                        Acc,
                        Errs1
                    )
                end,
                [],
                Errs
            )
    end;
ast_vars(AST) when is_tuple(AST) ->
    ast_vars([AST]).

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

normalize_expr(Expr) ->
    erlang:binary_to_list(erlang:iolist_to_binary([Expr, "."])).
