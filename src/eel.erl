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
    case eel_tokenizer:tokenize(Bin) of
        {ok, {Static, Dynamic}} ->
            AST = eel_compiler:compile(Dynamic),
            {Static, AST};
        {error, Reason} ->
            {error, Reason}
    end.

compile(Bin, Opts) ->
    case eel_tokenizer:tokenize(Bin, Opts) of
        {ok, {Static, Dynamic}} ->
            AST = eel_compiler:compile(Dynamic),
            {Static, AST};
        {error, Reason} ->
            {error, Reason}
    end.

compile_file(Filename) ->
    case eel_tokenizer:tokenize_file(Filename) of
        {ok, {Static, Dynamic}} ->
            AST = eel_compiler:compile(Dynamic),
            {Static, AST};
        {error, Reason} ->
            {error, Reason}
    end.

compile_file(Filename, Opts) ->
    case eel_tokenizer:tokenize_file(Filename, Opts) of
        {ok, {Static, Dynamic}} ->
            AST = eel_compiler:compile(Dynamic),
            {Static, AST};
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
            eel_compiler:compile_to_module(Filename, Tokens);
        {error, Reason} ->
            {error, Reason}
    end.

compile_file_to_module(Filename, Opts) ->
    case eel_tokenizer:tokenize_file(Filename, Opts) of
        {ok, Tokens} ->
            eel_compiler:compile_to_module(Filename, Tokens, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

render(Bin) ->
    case eel_tokenizer:tokenize(Bin) of
        {ok, {Static, Dynamic} } ->
            AST = eel_compiler:compile(Dynamic),
            eel_renderer:render({Static, AST});
        {error, Reason} ->
            {error, Reason}
    end.

render(Bin, Bindings) ->
    case eel_tokenizer:tokenize(Bin) of
        {ok, {Static, Dynamic} } ->
            AST = eel_compiler:compile(Dynamic),
            eel_renderer:render({Static, AST}, Bindings);
        {error, Reason} ->
            {error, Reason}
    end.

render(Bin, Bindings, Opts) ->
    case eel_tokenizer:tokenize(Bin, Opts) of
        {ok, {Static, Dynamic} } ->
            AST = eel_compiler:compile(Dynamic, Opts),
            eel_renderer:render({Static, AST}, Bindings, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

render_file(Filename) ->
    case eel_tokenizer:tokenize_file(Filename) of
        {ok, {Static, Dynamic} } ->
            AST = eel_compiler:compile(Dynamic),
            eel_renderer:render({Static, AST});
        {error, Reason} ->
            {error, Reason}
    end.

render_file(Filename, Bindings) ->
    case eel_tokenizer:tokenize_file(Filename) of
        {ok, {Static, Dynamic} } ->
            AST = eel_compiler:compile(Dynamic),
            eel_renderer:render({Static, AST}, Bindings);
        {error, Reason} ->
            {error, Reason}
    end.

render_file(Filename, Bindings, Opts) ->
    case eel_tokenizer:tokenize_file(Filename, Opts) of
        {ok, {Static, Dynamic} } ->
            AST = eel_compiler:compile(Dynamic, Opts),
            eel_renderer:render({Static, AST}, Bindings, Opts);
        {error, Reason} ->
            {error, Reason}
    end.
