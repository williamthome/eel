%%%-----------------------------------------------------------------------------
%%% @doc Embedded Erlang library.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel).

-export([
    compile/1,
    compile_file/1,
    compile_priv_file/2,
    render/2,
    render/3,
    eval/2,
    eval/3,
    compiled_to_module/2,
    bin_to_module/2,
    file_to_module/1,
    priv_file_to_module/2
]).

compile(Bin) ->
    eel_compile:binary(Bin).

compile_file(FileName) ->
    eel_compile:file(FileName).

compile_priv_file(App, FileName) ->
    eel_compile:priv_file(App, FileName).

render(Compiled, Bindings) ->
    eel_render:compiled(Compiled, Bindings).

render(Compiled, Memo, Bindings) ->
    eel_render:compiled(Compiled, Memo, Bindings).

eval(Bin, Bindings) ->
    eel_eval:binary(Bin, Bindings).

eval(Bin, Memo, Bindings) ->
    eel_eval:binary(Bin, Memo, Bindings).

compiled_to_module({Static, AST}, FileName) ->
    eel_output:to_module({Static, AST}, FileName).

bin_to_module(Bin, FileName) ->
    eel_output:to_module({binary, Bin}, FileName).

file_to_module(FileName) ->
    eel_output:to_module(file, FileName).

priv_file_to_module(App, FileName) ->
    eel_output:to_module({priv_file, App}, FileName).
