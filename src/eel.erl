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
         default_engine/0, default_engine_opts/0]).

%% Includes
-include("eel.hrl").
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
                    {Static, AST};
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
                    {Static, AST};
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
                    {Static, AST};
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
                    {Static, AST};
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
        {ok, {Static, Dynamic} } ->
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
        {ok, {Static, Dynamic} } ->
            case eel_compiler:compile(Dynamic) of
                {ok, AST} ->
                    eel_renderer:render(Bindings, {Static, AST}, Opts);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

default_engine() ->
    ?DEFAULT_ENGINE.

default_engine_opts() ->
    ?DEFAULT_ENGINE_OPTS.

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
