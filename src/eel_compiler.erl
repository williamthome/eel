%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl compiler module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_compiler).

%% API functions
-export([compile/1, compile/2, compile_to_module/2, compile_to_module/3,
         compile_file_to_module/2, compile_file_to_module/3,
         compile_file_to_module/4, dynamic_to_ast/1, ast_vars/1]).

%% Types
-export_type([options/0]).

%% Includes
-include("eel.hrl").
-include_lib("syntax_tools/include/merl.hrl").

%% Types
-type options() :: map().

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc compile/1.
%% @end
%% -----------------------------------------------------------------------------
-spec compile(eel_engine:dynamic()) -> {ok, eel_engine:ast()} | {error, term()}.

compile(Dynamic) ->
    compile(Dynamic, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc compile/2.
%% @end
%% -----------------------------------------------------------------------------
-spec compile(eel_engine:dynamic(), options()) -> {ok, eel_engine:ast()}
                                                  | {error, term()}.

compile(Dynamic, Opts) when is_list(Dynamic) ->
    Eng = maps:get(engine, Opts, ?DEFAULT_ENGINE),
    State = Eng:init(Opts),
    do_compile(Dynamic, Eng, State).

%% -----------------------------------------------------------------------------
%% @doc compile_to_module/2.
%% @end
%% -----------------------------------------------------------------------------
-spec compile_to_module(Tokens, Module) -> Return when
    Tokens :: eel_tokenizer:tokens(),
    Module :: module(),
    Return :: {ok, Module} | {error, term()}.

compile_to_module(Tokens, Module) ->
    compile_to_module(Tokens, Module, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc compile_to_module/3.
%% @end
%% -----------------------------------------------------------------------------
-spec compile_to_module(Tokens, Module, Options) -> Return when
    Tokens  :: eel_tokenizer:tokens(),
    Module  :: module(),
    Options :: options(),
    Return  :: {ok, Module} | {error, term()}.

compile_to_module(Tokens, Module, Opts) ->
    do_compile_to_module(Tokens, Module, Opts).

%% -----------------------------------------------------------------------------
%% @doc compile_file_to_module/2.
%% @end
%% -----------------------------------------------------------------------------
-spec compile_file_to_module(Filename, Tokens) -> Return when
    Filename :: file:filename_all(),
    Tokens   :: eel_tokenizer:tokens(),
    Return   :: {ok, module()} | {error, term()}.

compile_file_to_module(Filename, Tokens) ->
    compile_file_to_module(Filename, Tokens, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc compile_file_to_module/3.
%% @end
%% -----------------------------------------------------------------------------
-spec compile_file_to_module(Filename, Tokens, Options) -> Return when
    Filename :: file:filename_all(),
    Tokens   :: eel_tokenizer:tokens(),
    Options  :: options(),
    Return   :: {ok, module()} | {error, term()}.

compile_file_to_module(Filename, Tokens, Module) when is_atom(Module) ->
    compile_file_to_module(Filename, Tokens, Module, ?DEFAULT_ENGINE_OPTS);
compile_file_to_module(Filename, Tokens, Opts) when is_map(Opts) ->
    compile_file_to_module(Filename, Tokens, file_module(Filename), Opts).

%% -----------------------------------------------------------------------------
%% @doc compile_file_to_module/4.
%% @end
%% -----------------------------------------------------------------------------
-spec compile_file_to_module(Filename, Tokens, Module, Options) -> Return when
    Filename :: file:filename_all(),
    Tokens   :: eel_tokenizer:tokens(),
    Module   :: module(),
    Options  :: options(),
    Return   :: {ok, Module} | {error, term()}.

compile_file_to_module(Filename, Tokens, Module, Opts) ->
    do_compile_file_to_module(Filename, Tokens, Module, Opts).

%% -----------------------------------------------------------------------------
%% @doc dynamic_to_ast/1.
%% @end
%% -----------------------------------------------------------------------------
-spec dynamic_to_ast(binary() | string()) -> {ok, eel_engine:ast()}
                                             | {error, term()}.

dynamic_to_ast(Expr) ->
    case erl_scan:string(normalize_expr(Expr)) of
        {ok, Tokens, _} ->
            erl_parse:parse_exprs(Tokens);
        {error, ErrorInfo, ErrorLocation} ->
            {error, {ErrorInfo, ErrorLocation}}
    end.

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
    lists:reverse(
        lists:foldl(
            fun({Index, Tree}, Acc) ->
                Vars =
                    case erl_lint:exprs_opt(Tree, [], Opts) of
                        {ok, _Warns} ->
                            [];
                        {error, Errs, _Warns} ->
                            lists:foldl(
                                fun({"nofile", Errs1}, Acc1) ->
                                    lists:foldl(
                                        fun
                                            ({_Line, erl_lint, {unbound_var, Var}}, Acc2) ->
                                                [Var | Acc2];
                                            (_, Acc2) ->
                                                Acc2
                                        end,
                                        Acc1,
                                        Errs1
                                    )
                                end,
                                [],
                                Errs
                            )
                    end,
                [{Index, Vars} | Acc]
            end,
            [],
            lists:enumerate(AST)
        )
    );
ast_vars(AST) when is_tuple(AST) ->
    ast_vars([AST]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_compile([D | Dynamic], Eng, State) ->
    case Eng:handle_compile(D, State) of
        {ok, NewState} ->
            do_compile(Dynamic, Eng, NewState);
        {error, Reason} ->
            {error, Reason}
    end;
do_compile([], Eng, State) ->
    Eng:handle_ast(State).

do_compile_to_module(Tokens, Module, Opts) ->
    do_compile_file_to_module(undefined, Tokens, Module, Opts).

do_compile_file_to_module(Filename, {Static, Dynamic}, Module, Opts) ->
    case erlang:module_loaded(Module) of
        true ->
            {ok, Module};
        false ->
            case compile(Dynamic, Opts) of
                {ok, AST} ->
                    Vars = ast_vars(AST),
                    Forms = module_forms(Module, Filename, Static, AST, Vars, Opts),
                    {ok, Module, Bin} = compile:forms(Forms),
                    ModuleBin = atom_to_binary(Module),
                    ModuleFileName = erlang:binary_to_list(<<ModuleBin/binary, ".erl">>),
                    case code:load_binary(Module, ModuleFileName, Bin) of
                        {module, Module} ->
                            {ok, Module};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

module_forms(Module, Filename, Static, AST, Vars, Opts) ->
    Forms =
        ?Q(["-module('@module').",
            "",
            "-export([filename/0,",
            "         timestamp/0,",
            "         static/0,",
            "         ast/0,",
            "         vars/0,",
            "         compile_opts/0,",
            "         render/1,",
            "         render/2,",
            "         render/3]).",
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
            "render(Bindings, Opts0) when is_map(Opts0) ->",
            "    Snapshot0 = #{static => static(),",
            "                  ast => ast(),",
            "                  vars => vars()},",
            "    Opts = maps:merge(compile_opts(), Opts0),",
            "    {Render, Snapshot} = eel_renderer:render(Bindings, Snapshot0, Opts),",
            "    {Render, maps:with([dynamic, bindings], Snapshot)};",
            "render(Changes, Snapshot) ->",
            "    render(Changes, Snapshot, #{}).",
            "",
            "render(Changes, #{dynamic := DynamicSnap, bindings := BindingsSnap}, Opts0) ->",
            "    Snapshot0 = #{static => static(),",
            "                  ast => ast(),",
            "                  vars => vars(),",
            "                  dynamic => DynamicSnap,",
            "                  bindings => BindingsSnap},",
            "    Opts = maps:merge(compile_opts(), Opts0),",
            "    {Render, Snapshot} = eel_renderer:render(Changes, Snapshot0, Opts),",
            "    {Render, maps:with([dynamic, bindings], Snapshot)}.",
            ""],
            [{module, merl:term(Module)},
             {filename, merl:term(Filename)},
             {timestamp, merl:term(os:timestamp())},
             {static, merl:term(Static)},
             {ast, merl:term(AST)},
             {vars, merl:term(Vars)},
             {compile_opts, merl:term(Opts)}]),
    [erl_syntax:revert(Form) || Form <- Forms].

file_module(Filename) when is_binary(Filename); is_list(Filename) ->
    Basename =
        case filename:basename(Filename) of
            N when is_binary(N) -> N;
            N when is_list(N) -> list_to_binary(N)
        end,
    Name = binary:replace(Basename, <<".">>, <<"_">>, [global]),
    case catch erlang:binary_to_existing_atom(Name) of
        Atom when is_atom(Atom) -> Atom;
        _ -> erlang:binary_to_atom(Name)
    end.

normalize_expr(Expr) ->
    erlang:binary_to_list(erlang:iolist_to_binary([Expr, "."])).
