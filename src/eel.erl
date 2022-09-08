%%%---------------------------------------------------------------------------------------
%%% @doc Embedded Erlang library.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @end
%%%---------------------------------------------------------------------------------------
-module(eel).

-export([
    compile/1,
    compile_file/1,
    compile_priv_file/2,
    render/2,
    render/3,
    eval/2,
    eval/3
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
