-module(eel_test).

-include_lib("eunit/include/eunit.hrl").

mod_render_test() ->
    Expect = {ok, #{ ast      => [ [{call,1,
                                    {remote,1,{atom,1,eel_converter},{atom,1,to_binary}}
                                 , [{'fun',1,
                                    {clauses,[{clause,1,[],[],[{var,1,'Bar'}]}]}}]}] ]
                   , bindings => #{ 'Bar' => bar
                                  , 'Bindings' => #{'Bar' => bar} }
                   , dynamic  => [<<"bar">>]
                   , static   => [<<"Foo">>,<<"Baz">>]
                   , vars     => [{1, ['Bar']}] } },
    Expr = foo:render(#{}),
    ?assertEqual(Expect, Expr).
    
mod_eval_test() ->
    Expect = <<"FooBarBaz">>,
    Expr = foo:eval(#{bar => <<"Bar">>}),
    ?assertEqual(Expect, Expr).

