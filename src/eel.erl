%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Embedded Erlang library.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel).

-compile({inline, [ compile/2
                  , compile_file/2
                  , render/3
                  , render_file/3
                  ]}).

%% API functions
-export([ compile/1
        , compile/2
        , compile_file/1
        , compile_file/2
        , to_module/2
        , to_module/3
        , file_to_module/1
        , file_to_module/2
        , file_to_module/3
        , render/1
        , render/2
        , render/3
        , render_file/1
        , render_file/2
        , render_file/3
        , eval/1
        , eval/2
        , eval/3
        , eval_file/1
        , eval_file/2
        , eval_file/3
        , to_file/3
        , to_file/4
        , file_to_file/3
        , file_to_file/4
        , default_engine/0
        , default_engine_opts/0
        ]).

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
        {ok, {{Static, Dynamic}, State}} ->
            case eel_compiler:compile(Dynamic, Opts, State) of
                {ok, {AST, NewState}} ->
                    {ok, eel_snapshot:new(Static, AST, NewState)};
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
        {ok, {{Static, Dynamic}, State}} ->
            case eel_compiler:compile(Dynamic, Opts, State) of
                {ok, {AST, NewState}} ->
                    {ok, eel_snapshot:new(Static, AST, NewState)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

to_module(Bin, ModName) ->
    to_module(Bin, ModName, default_engine_opts()).

to_module(Bin, ModName, Opts) ->
    case compile(Bin, Opts) of
        {ok, Snapshot} ->
            eel_compiler:compile_to_module(Snapshot, ModName);
        {error, Reason} ->
            {error, Reason}
    end.

file_to_module(Filename) ->
    file_to_module(Filename, default_engine_opts()).

file_to_module(Filename, Module) when is_atom(Module) ->
    file_to_module(Filename, Module, default_engine_opts());
file_to_module(Filename, Opts) when is_map(Opts) ->
    Module = eel_converter:filename_to_module(Filename),
    file_to_module(Filename, Module, Opts).

file_to_module(Filename, Module, Opts) ->
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
        {ok, {{Static, Dynamic}, State}} ->
            case eel_compiler:compile(Dynamic, Opts, State) of
                {ok, {AST, NewState}} ->
                    Snapshot = eel_snapshot:new(Static, Dynamic, AST, NewState),
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
        {ok, {{Static, Dynamic}, State}} ->
            case eel_compiler:compile(Dynamic, Opts, State) of
                {ok, {AST, NewState}} ->
                    Snapshot = eel_snapshot:new(Static, Dynamic, AST, NewState),
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

to_file(Bin, Bindings, OutFilename) ->
    to_file(Bin, Bindings, OutFilename, ?DEFAULT_ENGINE_OPTS).

to_file(Bin, Bindings, OutFilename, Opts) ->
    Data = eval(Bin, Bindings, Opts),
    write_file(OutFilename, Data).

file_to_file(InFilename, Bindings, OutFilename) ->
    file_to_file(InFilename, Bindings, OutFilename, ?DEFAULT_ENGINE_OPTS).

file_to_file(InFilename, Bindings, OutFilename, Opts) ->
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

to_module_test() ->
    Bin = <<"Hello, World!">>,
    [ ?assertMatch({ok, foo}, to_module(Bin, foo))
    , ?assertMatch({ok, foo}, to_module(Bin, foo, #{}))
    ].

compile_file_to_module_test() ->
    Filename = "/tmp/foo.eel",
    ok = file:write_file(Filename, <<"Hello, World!">>),
    [ ?assertMatch({ok, foo_eel}, file_to_module(Filename))
    , ?assertMatch({ok, foo}, file_to_module(Filename, foo))
    , ?assertMatch({ok, foo_eel}, file_to_module(Filename, #{}))
    ].

-endif.
