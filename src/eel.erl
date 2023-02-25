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
    Tokens = eel_tokenizer:tokenize(Bin),
    eel_compiler:compile(Tokens).

compile(Bin, Opts) ->
    Tokens = eel_tokenizer:tokenize(Bin, Opts),
    eel_compiler:compile(Tokens, Opts).

compile_file(Filename) ->
    Tokens = eel_tokenizer:tokenize_file(Filename),
    eel_compiler:compile(Tokens).

compile_file(Filename, Opts) ->
    Tokens = eel_tokenizer:tokenize_file(Filename, Opts),
    eel_compiler:compile(Tokens, Opts).

compile_to_module(ModName, Bin) ->
    Tokens = eel_tokenizer:tokenize(Bin),
    eel_compiler:compile_to_module(ModName, Tokens).

compile_to_module(ModName, Bin, Opts) ->
    Tokens = eel_tokenizer:tokenize(Bin, Opts),
    eel_compiler:compile_to_module(ModName, Tokens, Opts).

compile_file_to_module(Filename) ->
    Tokens = eel_tokenizer:tokenize_file(Filename),
    eel_compiler:compile_to_module(Filename, Tokens).

compile_file_to_module(Filename, Opts) ->
    Tokens = eel_tokenizer:tokenize_file(Filename, Opts),
    eel_compiler:compile_to_module(Filename, Tokens, Opts).

render(Bin) ->
    Tokens = eel_tokenizer:tokenize(Bin),
    AST = eel_compiler:compile(Tokens),
    eel_renderer:render(AST).

render(Bin, Bindings) ->
    Tokens = eel_tokenizer:tokenize(Bin),
    AST = eel_compiler:compile(Tokens),
    eel_renderer:render(AST, Bindings).

render(Bin, Bindings, Opts) ->
    Tokens = eel_tokenizer:tokenize(Bin, Opts),
    AST = eel_compiler:compile(Tokens, Opts),
    eel_renderer:render(AST, Bindings, Opts).

render_file(Filename) ->
    Tokens = eel_tokenizer:tokenize_file(Filename),
    AST = eel_compiler:compile(Tokens),
    eel_renderer:render(AST).

render_file(Filename, Bindings) ->
    Tokens = eel_tokenizer:tokenize_file(Filename),
    AST = eel_compiler:compile(Tokens),
    eel_renderer:render(AST, Bindings).

render_file(Filename, Bindings, Opts) ->
    Tokens = eel_tokenizer:tokenize_file(Filename, Opts),
    AST = eel_compiler:compile(Tokens, Opts),
    eel_renderer:render(AST, Bindings, Opts).
