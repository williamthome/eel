%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Embedded Erlang library.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel).

-export([compile/1, compile/2,
         compile_file/1, compile_file/2,
         compile_to_module/2, compile_to_module/3,
         compile_file_to_module/1, compile_file_to_module/2,
         render/1, render/2, render/3,
         render_file/1, render_file/2, render_file/3]).

%% TODO: docs/specs

compile(Bin) ->
    {Static, Dynamic} = eel_tokenizer:tokenize(Bin),
    AST = eel_compiler:compile(Dynamic),
    {Static, AST}.

compile(Bin, Opts) ->
    {Static, Dynamic} = eel_tokenizer:tokenize(Bin, Opts),
    AST = eel_compiler:compile(Dynamic, Opts),
    {Static, AST}.

compile_file(Filename) ->
    {Static, Dynamic} = eel_tokenizer:tokenize_file(Filename),
    AST = eel_compiler:compile(Dynamic),
    {Static, AST}.

compile_file(Filename, Opts) ->
    {Static, Dynamic} = eel_tokenizer:tokenize_file(Filename, Opts),
    AST = eel_compiler:compile(Dynamic, Opts),
    {Static, AST}.

compile_to_module(Bin, ModName) ->
    Tokens = eel_tokenizer:tokenize(Bin),
    eel_compiler:compile_to_module(Tokens, ModName).

compile_to_module(Bin, ModName, Opts) ->
    Tokens = eel_tokenizer:tokenize(Bin, Opts),
    eel_compiler:compile_to_module(Tokens, ModName, Opts).

compile_file_to_module(Filename) ->
    Tokens = eel_tokenizer:tokenize_file(Filename),
    eel_compiler:compile_to_module(Filename, Tokens).

compile_file_to_module(Filename, Opts) ->
    Tokens = eel_tokenizer:tokenize_file(Filename, Opts),
    eel_compiler:compile_to_module(Filename, Tokens, Opts).

render(Bin) ->
    {Static, Dynamic} = eel_tokenizer:tokenize(Bin),
    AST = eel_compiler:compile(Dynamic),
    eel_renderer:render({Static, AST}).

render(Bin, Bindings) ->
    {Static, Dynamic} = eel_tokenizer:tokenize(Bin),
    AST = eel_compiler:compile(Dynamic),
    eel_renderer:render({Static, AST}, Bindings).

render(Bin, Bindings, Opts) ->
    {Static, Dynamic} = eel_tokenizer:tokenize(Bin, Opts),
    AST = eel_compiler:compile(Dynamic, Opts),
    eel_renderer:render({Static, AST}, Bindings, Opts).

render_file(Filename) ->
    {Static, Dynamic} = eel_tokenizer:tokenize_file(Filename),
    AST = eel_compiler:compile(Dynamic),
    eel_renderer:render({Static, AST}).

render_file(Filename, Bindings) ->
    {Static, Dynamic} = eel_tokenizer:tokenize_file(Filename),
    AST = eel_compiler:compile(Dynamic),
    eel_renderer:render({Static, AST}, Bindings).

render_file(Filename, Bindings, Opts) ->
    {Static, Dynamic} = eel_tokenizer:tokenize_file(Filename, Opts),
    AST = eel_compiler:compile(Dynamic, Opts),
    eel_renderer:render({Static, AST}, Bindings, Opts).
