-module(v2_eel_renderer).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

% render_test() ->
%     _Snapshot = #{
%         tokens => #{
%             statics => [
%                 <<"Hello ">>, <<"!">>
%             ],
%             dynamics = #{
%                 0 => #{
%                     expr => fun(Bindings) -> maps:get(world, Bindings) end,
%                     vars => [world],
%                     engine => v2_eel_smart_engine
%                 }
%             },
%             indexes => #{
%                 vars => #{
%                     world => [0]
%                 }
%             }
%         }
%     },
%     ok.

render_test() ->
    Tree = #{
        vertices => #{
            0 => #{
                % children => [
                %     1, 2, 3
                % ],
                % ast => all_children,
                statics => [<<"Hello, ">>, <<"! From ">>],
                dynamics => [
                    #{
                        vertex => 0,
                        index => 1,
                        expr => <<"maps:get(world, Bindings)">>,
                        vars => [world],
                        ast => [
                            {call,1,
                            {remote,1,{atom,1,maps},{atom,1,get}},
                            [{atom,1,world},{var,1,'Bindings'}]}
                        ],
                        cache => <<"World">>
                    },
                    #{
                        index => 3,
                        vertex => 1
                    }
                ], % = AST
                cache => #{
                    render => [<<"Hello, ">>, <<"World">>, <<"!">>],
                    vars => #{
                        world => <<"World">>
                    }
                }
            },
            1 => #{
                statics => [<<>>, <<"!">>],
                dynamics => [
                    #{
                        vertex => 1,
                        index => 1,
                        expr => <<"maps:get(from, Bindings)">>,
                        vars => [from],
                        ast => [
                            {call,1,
                            {remote,1,{atom,1,maps},{atom,1,get}},
                            [{atom,1,from},{var,1,'Bindings'}]}
                        ]
                    }
                ]
            }
        }
    },
    Bindings = #{
        world => <<"World">>,
        from => <<"Erlang">>
    },
    RootLabel = 0,
    Render = render_vertex(RootLabel, Tree, Bindings),
    ?debugFmt("~p", [iolist_to_binary(Render)]),

    ok.

-endif.

render_vertex(VertexLabel, Tree, Bindings) ->
    Vertices = maps:get(vertices, Tree),
    Vertex = maps:get(VertexLabel, Vertices),
    do_render_vertex(Vertex, Tree, Bindings).

do_render_vertex(Vertex, Tree, Bindings) ->
    Statics = maps:get(statics, Vertex),
    Dynamics = maps:get(dynamics, Vertex),
    render(Statics, Dynamics, Tree, Bindings).

render(Statics, Dynamics0, Tree, Bindings) ->
    Dynamics = render_dynamics(Dynamics0, Tree, Bindings),
    zip(Statics, Dynamics).

zip(Statics, Dynamics) ->
    do_zip(Statics, Dynamics, []).

do_zip([S | Statics], [D | Dynamics], Acc) ->
    do_zip(Statics, Dynamics, [D, S | Acc]);
do_zip([S | Statics], [], Acc) ->
    do_zip(Statics, [], [S | Acc]);
do_zip([], [], Acc) ->
    lists:reverse(Acc);
do_zip([], Dynamics, Acc) ->
    lists:reverse(Dynamics, Acc).

render_dynamics(Dynamics, Tree, Bindings) ->
    lists:map(fun(Dynamic) -> render_dynamic(Dynamic, Tree, Bindings) end, Dynamics).

render_dynamic(#{ast := AST}, _, Bindings) ->
    {value, IOList, _} = erl_eval:exprs(AST, #{'Bindings' => Bindings}),
    IOList;
render_dynamic(#{vertex := VertexLabel}, Tree, Bindings) ->
    render_vertex(VertexLabel, Tree, Bindings).
