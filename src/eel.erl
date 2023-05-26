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
    compile(Bin, default_engine_opts()).

compile(Bin, Opts) ->
    case eel_tokenizer:tokenize(Bin, Opts) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic, Opts) of
                {ok, AST} ->
                    {ok, eel_renderer:snapshot(Static, AST)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

compile_file(Filename) ->
    compile_file(Filename, default_engine_opts()).

compile_file(Filename, Opts) ->
    case eel_tokenizer:tokenize_file(Filename, Opts) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic, Opts) of
                {ok, AST} ->
                    {ok, eel_renderer:snapshot(Static, AST)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

compile_to_module(Bin, ModName) ->
    compile_to_module(Bin, ModName, default_engine_opts()).

compile_to_module(Bin, ModName, Opts) ->
    case compile(Bin, Opts) of
        {ok, Snapshot} ->
            eel_compiler:compile_to_module(Snapshot, ModName);
        {error, Reason} ->
            {error, Reason}
    end.

compile_file_to_module(Filename) ->
    compile_file_to_module(Filename, default_engine_opts()).

compile_file_to_module(Filename, Module) when is_atom(Module) ->
    compile_file_to_module(Filename, Module, default_engine_opts());
compile_file_to_module(Filename, Opts) when is_map(Opts) ->
    Module = eel_compiler:file_module(Filename),
    compile_file_to_module(Filename, Module, Opts).

compile_file_to_module(Filename, Module, Opts) ->
    case compile_file(Filename, Opts) of
        {ok, Snapshot} ->
            eel_compiler:compile_file_to_module(Filename, Snapshot, Module);
        {error, Reason} ->
            {error, Reason}
    end.

render(Bin) ->
    render(Bin, #{}).

render(Bin, Bindings) ->
    render(Bin, Bindings, default_engine_opts()).

render(Bin, Bindings, Opts) ->
    case eel_tokenizer:tokenize(Bin, Opts) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic, Opts) of
                {ok, AST} ->
                    Snapshot = eel_renderer:snapshot(Static, Dynamic, AST),
                    eel_renderer:render(Bindings, Snapshot, Opts);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

render_file(Filename) ->
    render_file(Filename, #{}).

render_file(Filename, Bindings) ->
    render_file(Filename, Bindings, default_engine_opts()).

render_file(Filename, Bindings, Opts) ->
    case eel_tokenizer:tokenize_file(Filename, Opts) of
        {ok, {Static, Dynamic}} ->
            case eel_compiler:compile(Dynamic, Opts) of
                {ok, AST} ->
                    Snapshot = eel_renderer:snapshot(Static, Dynamic, AST),
                    eel_renderer:render(Bindings, Snapshot, Opts);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

eval(Bin) ->
    eval(Bin, #{}).

eval(Bin, Bindings) ->
    eval(Bin, Bindings, default_engine_opts()).

eval(Bin, Bindings, Opts) ->
    {ok, Snapshot} = render(Bin, Bindings, Opts),
    eel_evaluator:eval(Snapshot).

eval_file(Filename) ->
    eval_file(Filename, #{}).

eval_file(Filename, Bindings) ->
    eval_file(Filename, Bindings, default_engine_opts()).

eval_file(Filename, Bindings, Opts) ->
    {ok, Snapshot} = render_file(Filename, Bindings, Opts),
    eel_evaluator:eval(Snapshot).

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
