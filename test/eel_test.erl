-module(eel_test).

-include_lib("eunit/include/eunit.hrl").

compile_file_test() ->
    Template = eel_test_support:get_template(<<"foo.html.eel">>),
    Compiled = eel:compile_file(Template),
    ExpectedCompiled = {
        [<<"<h1>">>, <<"</h1>\n<h2>">>, <<"</h2>\n\n">>],
        [
            {[{var, 1, 'Foo'}], ['Foo']},
            {[{var, 1, 'Bar'}], ['Bar']}
        ]
    },
    ?assertEqual(ExpectedCompiled, Compiled),
    Bindings = #{
        'Foo' => foo,
        'Bar' => <<"bar">>
    },
    {HTML, _, _} = eel:render(Compiled, Bindings),
    ExpectedHTML = <<"<h1>foo</h1>\n<h2>bar</h2>\n\n">>,
    ?assertEqual(ExpectedHTML, HTML).

file_to_module_test() ->
    FileName = <<"greetings.html.eel">>,
    Template = eel_test_support:get_template(FileName),
    {ok, Module} = eel:file_to_module(Template),
    ?assertEqual(greetings_html_eel, Module).
