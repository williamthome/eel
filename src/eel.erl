%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Embedded Erlang library.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel).

-export([compile/1, compile/2,
         compile_file/1, compile_file/2,
         render/1, render/2, render/3,
         render_file/1, render_file/2, render_file/3]).

%% TODO: docs/specs

compile(Bin) ->
    Tokens = eel_tokenizer:tokenize(Bin),
    eel_tokenizer:compile(Tokens).

compile(Bin, Opts) ->
    Tokens = eel_tokenizer:tokenize(Bin, Opts),
    eel_tokenizer:compile(Tokens, Opts).

compile_file(Filename) ->
    Tokens = eel_tokenizer:tokenize_file(Filename),
    eel_tokenizer:compile(Tokens).

compile_file(Filename, Opts) ->
    Tokens = eel_tokenizer:tokenize_file(Filename, Opts),
    eel_tokenizer:compile(Tokens, Opts).

render(Bin) ->
    Tokens = eel_tokenizer:tokenize(Bin),
    AST = eel_tokenizer:compile(Tokens),
    eel_tokenizer:render(AST).

render(Bin, Bindings) ->
    Tokens = eel_tokenizer:tokenize(Bin),
    AST = eel_tokenizer:compile(Tokens),
    eel_tokenizer:render(AST, Bindings).

render(Bin, Bindings, Opts) ->
    Tokens = eel_tokenizer:tokenize(Bin, Opts),
    AST = eel_tokenizer:compile(Tokens, Opts),
    eel_tokenizer:render(AST, Bindings, Opts).

render_file(Filename) ->
    Tokens = eel_tokenizer:tokenize_file(Filename),
    AST = eel_tokenizer:compile(Tokens),
    eel_tokenizer:render(AST).

render_file(Filename, Bindings) ->
    Tokens = eel_tokenizer:tokenize_file(Filename),
    AST = eel_tokenizer:compile(Tokens),
    eel_tokenizer:render(AST, Bindings).

render_file(Filename, Bindings, Opts) ->
    Tokens = eel_tokenizer:tokenize_file(Filename, Opts),
    AST = eel_tokenizer:compile(Tokens, Opts),
    eel_tokenizer:render(AST, Bindings, Opts).
