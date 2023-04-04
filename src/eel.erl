%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Embedded Erlang library.
%%% TODO: docs/specs and tests
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel).

%% API functions
-export([compile/1, compile/2, compile_file/1, compile_file/2,
         compile_to_module/2, compile_to_module/3, compile_file_to_module/1,
         compile_file_to_module/2, compile_file_to_module/3, render/1, render/2,
         render/3, render_file/1, render_file/2, render_file/3,
         eval/1, eval/2, eval/3, eval_file/1, eval_file/2, eval_file/3,
         eval_to_file/2, eval_to_file/3, eval_to_file/4,
         eval_file_to_file/2, eval_file_to_file/3, eval_file_to_file/4,
         default_engine/0, default_engine_opts/0]).

%% Includes
-include("eel_core.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% API functions
%%%=============================================================================

compile(Bin) ->
    case eel_tokenizer:tokenize(Bin) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic) of
                {ok, AST} ->
                    {ok, {Static, AST}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

compile(Bin, Opts) ->
    case eel_tokenizer:tokenize(Bin, Opts) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic) of
                {ok, AST} ->
                    {ok, {Static, AST}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

compile_file(Filename) ->
    case eel_tokenizer:tokenize_file(Filename) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic) of
                {ok, AST} ->
                    {ok, {Static, AST}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

compile_file(Filename, Opts) ->
    case eel_tokenizer:tokenize_file(Filename, Opts) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic) of
                {ok, AST} ->
                    {ok, {Static, AST}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

compile_to_module(Bin, ModName) ->
    case eel_tokenizer:tokenize(Bin) of
        {ok, Tokens} ->
            eel_compiler:compile_to_module(Tokens, ModName);
        {error, Reason} ->
            {error, Reason}
    end.

compile_to_module(Bin, ModName, Opts) ->
    case eel_tokenizer:tokenize(Bin, Opts) of
        {ok, Tokens} ->
            eel_compiler:compile_to_module(Tokens, ModName, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

compile_file_to_module(Filename) ->
    case eel_tokenizer:tokenize_file(Filename) of
        {ok, Tokens} ->
            eel_compiler:compile_file_to_module(Filename, Tokens);
        {error, Reason} ->
            {error, Reason}
    end.

compile_file_to_module(Filename, Module) when is_atom(Module) ->
    case eel_tokenizer:tokenize_file(Filename) of
        {ok, Tokens} ->
            eel_compiler:compile_file_to_module(Filename, Tokens, Module);
        {error, Reason} ->
            {error, Reason}
    end;
compile_file_to_module(Filename, Opts) when is_map(Opts) ->
    case eel_tokenizer:tokenize_file(Filename, Opts) of
        {ok, Tokens} ->
            eel_compiler:compile_file_to_module(Filename, Tokens, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

compile_file_to_module(Filename, Module, Opts) ->
    case eel_tokenizer:tokenize_file(Filename, Opts) of
        {ok, Tokens} ->
            eel_compiler:compile_file_to_module(Filename, Tokens, Module, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

render(Bin) ->
    case eel_tokenizer:tokenize(Bin) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic) of
                {ok, AST} ->
                    eel_renderer:render({Static, AST});
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

render(Bin, Bindings) ->
    case eel_tokenizer:tokenize(Bin) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic) of
                {ok, AST} ->
                    eel_renderer:render(Bindings, {Static, AST});
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

render(Bin, Bindings, Opts) ->
    case eel_tokenizer:tokenize(Bin, Opts) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic) of
                {ok, AST} ->
                    eel_renderer:render(Bindings, {Static, AST}, Opts);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

render_file(Filename) ->
    case eel_tokenizer:tokenize_file(Filename) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic) of
                {ok, AST} ->
                    eel_renderer:render({Static, AST});
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

render_file(Filename, Bindings) ->
    case eel_tokenizer:tokenize_file(Filename) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic) of
                {ok, AST} ->
                    eel_renderer:render(Bindings, {Static, AST});
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

render_file(Filename, Bindings, Opts) ->
    case eel_tokenizer:tokenize_file(Filename, Opts) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic) of
                {ok, AST} ->
                    eel_renderer:render(Bindings, {Static, AST}, Opts);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

eval(Bin) ->
    eel_evaluator:eval(render(Bin)).

eval(Bin, Bindings) ->
    eel_evaluator:eval(render(Bin, Bindings)).

eval(Bin, Bindings, Opts) ->
    eel_evaluator:eval(render(Bin, Bindings, Opts)).

eval_file(Filename) ->
    eel_evaluator:eval(render_file(Filename)).

eval_file(Filename, Bindings) ->
    eel_evaluator:eval(render_file(Filename, Bindings)).

eval_file(Filename, Bindings, Opts) ->
    eel_evaluator:eval(render_file(Filename, Bindings, Opts)).

eval_to_file(Bin, OutFilename) ->
    eval_to_file(Bin, #{}, OutFilename).

eval_to_file(Bin, Bindings, OutFilename) ->
    eval_to_file(Bin, Bindings, OutFilename, ?DEFAULT_ENGINE_OPTS).

eval_to_file(Bin, Bindings, OutFilename, Opts) ->
    Data = eval(Bin, Bindings, Opts),
    write_file(OutFilename, Data).

eval_file_to_file(InFilename, OutFilename) ->
    eval_file_to_file(InFilename, #{}, OutFilename).

eval_file_to_file(InFilename, Bindings, OutFilename) ->
    eval_file_to_file(InFilename, Bindings, OutFilename, ?DEFAULT_ENGINE_OPTS).

eval_file_to_file(InFilename, Bindings, OutFilename, Opts) ->
    Data = eval_file(InFilename, Bindings, Opts),
    write_file(OutFilename, Data).

default_engine() ->
    ?DEFAULT_ENGINE.

default_engine_opts() ->
    ?DEFAULT_ENGINE_OPTS.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

write_file(Filename, Data) ->
    case file:write_file(Filename, Data) of
        ok ->
            {ok, Data};
        {error, Reason} ->
            {error, Reason}
    end.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

compile_to_module_test() ->
    Bin = <<"Hello, World!">>,
    [?assertMatch({ok, foo}, compile_to_module(Bin, foo)),
     ?assertMatch({ok, foo}, compile_to_module(Bin, foo, #{}))].

compile_file_to_module_test() ->
    Filename = "/tmp/foo.eel",
    ok = file:write_file(Filename, <<"Hello, World!">>),
    [?assertMatch({ok, foo_eel}, compile_file_to_module(Filename)),
     ?assertMatch({ok, foo}, compile_file_to_module(Filename, foo)),
     ?assertMatch({ok, foo_eel}, compile_file_to_module(Filename, #{}))].

-endif.

