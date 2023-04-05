-module(foo).

-include("eel.hrl").

-export([render/1, eval/1]).

render(Bindings0) ->
    Bindings = Bindings0#{bar => maps:get(bar, Bindings0, bar)},
    ?eel_bin_render("Foo<%= Bar .%>Baz").

eval(Bindings) ->
    ?eel_bin_eval("Foo<%= Bar .%>Baz").

% FIXME: eel_test_support:get_template/1 is undefined via eel_transform in tests

% file_render(Bindings) ->
%     ?eel_file_render(eel_test_support:get_template("foo.eel")).
%
% file_eval(Bindings) ->
%     ?eel_file_eval(eel_test_support:get_template("foo.eel")).

