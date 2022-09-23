-module(eel_test_support).

-include_lib("eunit/include/eunit.hrl").

-export([
    compile_file/1,
    get_template/1
]).

compile_file(Filename) ->
    Template = get_template(Filename),
    eel:compile_file(Template).

get_template(Filename) ->
    get_support_file(["templates", Filename]).

get_support_file(Components) ->
    filename:join([get_cwd(), "test", "support", filename:join(Components)]).

get_cwd() ->
    {ok, CWD} = file:get_cwd(),
    CWD.
