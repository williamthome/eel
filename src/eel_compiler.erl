%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl compiler module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_compiler).

-compile(inline_list_funcs).
-compile({inline, [ dynamic_to_ast/1
                  , ast_vars/1
                  , do_compile/4
                  , do_compile_file_to_module/3
                  ]}).

%% API functions
-export([ compile/1
        , compile/2
        , compile_to_module/2
        , compile_file_to_module/3
        , dynamic_to_ast/1
        , ast_vars/1
        ]).

%% Types
-export_type([ options/0 ]).

%% Includes
-include("eel_core.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Types
-type options()  :: map().
-type dynamic()  :: eel_engine:dynamic().
-type ast()      :: eel_engine:ast().
-type index()    :: eel_engine:index().
-type snapshot() :: eel_snapshot:snapshot().
-type position() :: eel_engine:position().

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc compile/1.
%% @end
%% -----------------------------------------------------------------------------
-spec compile(Dynamic) -> Result
    when Dynamic :: dynamic()
       , Result  :: {ok, ast()} | {error, term()}
       .

compile(Dynamic) ->
    compile(Dynamic, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc compile/2.
%% @end
%% -----------------------------------------------------------------------------
-spec compile(Dynamic, Opts) -> Result
    when Dynamic :: dynamic()
       , Opts    :: options()
       , Result  :: {ok, ast()} | {error, term()}
       .

compile(Dynamic, Opts) when is_list(Dynamic) ->
    Eng = maps:get(engine, Opts, ?DEFAULT_ENGINE),
    case Eng:init(Opts) of
        {ok, State} ->
            do_compile(Dynamic, Eng, State, []);
        {error, Reason} ->
            {error, Reason}
    end.

%% -----------------------------------------------------------------------------
%% @doc compile_to_module/2.
%% @end
%% -----------------------------------------------------------------------------
-spec compile_to_module(Snapshot, Module) -> Return
    when Snapshot :: snapshot()
       , Module   :: module()
       , Return   :: {ok, Module} | {error, term()}
       .

compile_to_module(Snapshot, Module) ->
    do_compile_to_module(Snapshot, Module).

%% -----------------------------------------------------------------------------
%% @doc compile_file_to_module/3.
%% @end
%% -----------------------------------------------------------------------------
-spec compile_file_to_module(Filename, Snapshot, Module) -> Return
    when Filename :: file:filename_all()
       , Snapshot :: snapshot()
       , Module   :: module()
       , Return   :: {ok, module()} | {error, term()}
       .

compile_file_to_module(Filename, Snapshot, Module) when is_atom(Module) ->
    do_compile_file_to_module(Filename, Snapshot, Module).

%% -----------------------------------------------------------------------------
%% @doc dynamic_to_ast/1.
%% @end
%% -----------------------------------------------------------------------------
-spec dynamic_to_ast(Expr) -> Result
    when Expr   :: {index(), {position(), binary() | string()}}
       , Result :: {ok, ast()} | {error, term()}
       .

dynamic_to_ast({Index, {Pos, Expr}}) ->
    case erl_scan:string(normalize_expr(Expr)) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, AST} ->
                    {ok, {Index, {Pos, AST}}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, ErrorInfo, ErrorLocation} ->
            {error, {ErrorInfo, ErrorLocation}}
    end.

%% -----------------------------------------------------------------------------
%% @doc ast_vars/1.
%% @end
%% -----------------------------------------------------------------------------
-spec ast_vars(AST) -> Result
    when AST    :: ast()
       , Result :: [atom()]
       .

ast_vars(AST) when is_list(AST) ->
    Opts = [ nowarn_underscore_match
           , nowarn_export_all
           , nowarn_export_vars
           , nowarn_shadow_vars
           , nowarn_unused_import
           , nowarn_unused_function
           , nowarn_unused_type
           , nowarn_bif_clash
           , nowarn_unused_record
           , nowarn_deprecated_function
           , nowarn_deprecated_type
           , nowarn_obsolete_guard
           , nowarn_untyped_record
           , nowarn_missing_spec
           , nowarn_missing_spec_all
           , nowarn_removed
           , nowarn_nif_inline
           , nowarn_keywords
           ],
    lists:reverse(
        lists:foldl(
            fun({Index, {_Pos, Forms}}, Acc) ->
                Vars =
                    lists:foldl(
                        fun({"nofile", Errs}, Acc1) ->
                            lists:foldl(
                                fun(Err, Acc2) ->
                                    case is_unbound_var_err(Err) of
                                        {true, Var} ->
                                            [Var | Acc2];
                                        false ->
                                            Acc2
                                    end
                                end,
                                Acc1,
                                Errs
                            )
                        end,
                        [],
                        get_forms_errs(Forms, Opts)
                    ),
                [{Index, lists:reverse(Vars)} | Acc]
            end,
            [],
            AST
        )
    );
ast_vars(AST) when is_tuple(AST) ->
    ast_vars([AST]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_compile([H | T], Eng, State, Acc) ->
    case Eng:handle_compile(H, State) of
        {ok, {Dynamic, NewState}} ->
            case eel_compiler:dynamic_to_ast(Dynamic) of
                {ok, AST} ->
                    do_compile(T, Eng, NewState, [AST | Acc]);
                {error, Reason} ->
                    {error, Reason}
                end;
        {error, Reason} ->
            {error, Reason}
    end;
do_compile([], Eng, State, AST) ->
    Eng:handle_ast(lists:reverse(AST), State).

do_compile_to_module(Snapshot, Module) ->
    do_compile_file_to_module(undefined, Snapshot, Module).

% TODO: Check if we should skip the module_loaded verification.
%       Maybe the module should be updated with the new snapshot.
do_compile_file_to_module(Filename, Snapshot, Module) ->
    case erlang:module_loaded(Module) of
        true ->
            {ok, Module};
        false ->
            Forms = module_forms(Module, Filename, Snapshot),
            {ok, Module, Bin} = compile:forms(Forms),
            case code:load_binary(Module, Module, Bin) of
                {module, Module} ->
                    {ok, Module};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

module_forms(Module, Filename, Snapshot) ->
    Forms =
        merl:qquote(
            [ "-module('@module')."
            , ""
            , "-export([ filename/0"
            , "        , timestamp/0"
            , "        , snapshot/0"
            , "        , render/1"
            , "        , render/2"
            , "        , eval/1"
            , "        , eval/2"
            , "        ])."
            , ""
            , "filename() -> _@filename."
            , ""
            , "timestamp() -> _@timestamp."
            , ""
            , "snapshot() -> _@snapshot."
            , ""
            , "render(Bindings) ->"
            , "    render(Bindings, snapshot())."
            , ""
            , "render(Bindings, Snapshot) ->"
            , "    {ok, RenderSnapshot} = eel_renderer:render(Bindings, Snapshot),"
            , "    {eel_evaluator:eval(RenderSnapshot), RenderSnapshot}."
            , ""
            , "eval(Bindings) ->"
            , "    eval(Bindings, snapshot())."
            , ""
            , "eval(Bindings, Snapshot) ->"
            , "    {ok, RenderSnapshot} = eel_renderer:render(Bindings, Snapshot),"
            , "    eel_evaluator:eval(RenderSnapshot)."
            , ""
            ],
            [ {module,    merl:term(Module)}
            , {filename,  merl:term(Filename)}
            , {timestamp, merl:term(os:timestamp())}
            , {snapshot,  merl:term(Snapshot)}
            ]),
    [erl_syntax:revert(Form) || Form <- Forms].

normalize_expr(Expr) ->
    erlang:binary_to_list(erlang:iolist_to_binary([Expr, "."])).

get_forms_errs(Forms, Opts) ->
    case erl_lint:exprs_opt(Forms, [], Opts) of
        {ok, _Warns} ->
            [];
        {error, Errs, _Warns} ->
            Errs
    end.

is_unbound_var_err({_Line, erl_lint, {unbound_var, Var}}) ->
    {true, Var};
is_unbound_var_err(_) ->
    false.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

ast_vars_test() ->
    {ok, {_, Dynamic}} = eel_tokenizer:tokenize(<<
        "<%= Foo .%>"
        "<%= Foo = Bar, Foo .%>"
        "<%= [Foo, Bar] .%>"
    >>),
    {ok, AST} = compile(Dynamic),
    Expected = [{1, ['Foo']}, {2, ['Bar']}, {3, ['Foo', 'Bar']}],
    ?assertEqual(Expected, ast_vars(AST)).

-endif.
