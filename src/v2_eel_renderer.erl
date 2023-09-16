-module(v2_eel_renderer).

-export([render_vertex/3]).

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
    % Tree = #{
    %     vertices => #{
    %         0 => #{
    %             % children => [
    %             %     1, 2, 3
    %             % ],
    %             % ast => all_children,
    %             statics => [<<"Hello, ">>, <<"! From ">>],
    %             dynamics => [
    %                 #{
    %                     index => 1,
    %                     vertex => 0,
    %                     expr => <<"maps:get(world, Bindings)">>,
    %                     vars => [world],
    %                     ast => [
    %                         {call,1,
    %                         {remote,1,{atom,1,maps},{atom,1,get}},
    %                         [{atom,1,world},{var,1,'Bindings'}]}
    %                     ],
    %                     cache => <<"World">>
    %                 },
    %                 #{
    %                     index => 3,
    %                     vertex => 1
    %                 }
    %             ], % = AST
    %             cache => #{
    %                 render => [<<"Hello, ">>, <<"World">>, <<"!">>],
    %                 vars => #{
    %                     world => <<"World">>
    %                 }
    %             }
    %         },
    %         1 => #{
    %             statics => [<<>>, <<"!">>],
    %             dynamics => [
    %                 #{
    %                     vertex => 1,
    %                     index => 1,
    %                     expr => <<"maps:get(from, Bindings)">>,
    %                     vars => [from],
    %                     ast => [
    %                         {call,1,
    %                         {remote,1,{atom,1,maps},{atom,1,get}},
    %                         [{atom,1,from},{var,1,'Bindings'}]}
    %                     ]
    %                 }
    %             ]
    %         }
    %     }
    % },
    % Bindings = #{
    %     world => <<"World">>,
    %     from => <<"Erlang">>
    % },
    Tree = #{vertices =>
        #{0 =>
           #{statics => [<<"</p>">>,<<"!<p>">>,<<"Hello, ">>],
             dynamics =>
              [#{index => 3,vertex => 4},
               #{index => 1,expr => <<"maps:get(world, Bindings)">>,vertex => 2,
                 vars => [world],
                 ast =>
                  [{call,1,
                    {remote,1,{atom,1,maps},{atom,1,get}},
                    [{atom,1,world},{var,1,'Bindings'}]}]}]},
          4 =>
           #{statics => [],
             dynamics =>
              [#{index => 0,
                 expr =>
                  <<"case maps:get(bool, Bindings) of true -> v2_eel_renderer:render_vertex(6, Tree, Bindings) ; false -> v2_eel_renderer:render_vertex(9, Tree, Bindings) end">>,
                 vertex => 5,
                 vars => [bool],
                 ast =>
                  [{'case',1,
                    {call,1,
                     {remote,1,{atom,1,maps},{atom,1,get}},
                     [{atom,1,bool},{var,1,'Bindings'}]},
                    [{clause,1,
                      [{atom,1,true}],
                      [],
                      [{call,1,
                        {remote,1,{atom,1,v2_eel_renderer},{atom,1,render_vertex}},
                        [{integer,1,6},{var,1,'Tree'},{var,1,'Bindings'}]}]},
                     {clause,1,
                      [{atom,1,false}],
                      [],
                      [{call,1,
                        {remote,1,{atom,1,v2_eel_renderer},{atom,1,render_vertex}},
                        [{integer,1,9},{var,1,'Tree'},{var,1,'Bindings'}]}]}]}]}]},
          6 => #{statics => [<<"True">>],dynamics => []},
          9 => #{statics => [],dynamics => [#{index => 0,vertex => 10}]},
          10 =>
           #{statics => [],
             dynamics =>
              [#{index => 0,
                 expr =>
                  <<"case maps:get(foo, Bindings) of foo -> v2_eel_renderer:render_vertex(12, Tree, Bindings) ; _ -> v2_eel_renderer:render_vertex(15, Tree, Bindings) end">>,
                 vertex => 11,
                 vars => [foo],
                 ast =>
                  [{'case',1,
                    {call,1,
                     {remote,1,{atom,1,maps},{atom,1,get}},
                     [{atom,1,foo},{var,1,'Bindings'}]},
                    [{clause,1,
                      [{atom,1,foo}],
                      [],
                      [{call,1,
                        {remote,1,{atom,1,v2_eel_renderer},{atom,1,render_vertex}},
                        [{integer,1,12},{var,1,'Tree'},{var,1,'Bindings'}]}]},
                     {clause,1,
                      [{var,1,'_'}],
                      [],
                      [{call,1,
                        {remote,1,{atom,1,v2_eel_renderer},{atom,1,render_vertex}},
                        [{integer,1,15},{var,1,'Tree'},{var,1,'Bindings'}]}]}]}]}]},
          12 => #{statics => [<<"Foo">>],dynamics => []},
          15 =>
           #{statics => [<<"</p>">>,<<"<p>">>],
             dynamics =>
              [#{index => 1,expr => <<"maps:get(bar, Bindings)">>,vertex => 17,
                 vars => [bar],
                 ast =>
                  [{call,1,
                    {remote,1,{atom,1,maps},{atom,1,get}},
                    [{atom,1,bar},{var,1,'Bindings'}]}]}]}}},
    Bindings = #{world => <<"World">>, bool => false, foo => bar, bar => <<"baz">>},
    RootLabel = 0, % NOTE: We can render any vertex
    Render = render_vertex(RootLabel, Tree, Bindings),
    % Render = render_vertices([12,15], Tree, Bindings),
    ?debugFmt("~p", [Render]),

    ok.

-endif.

render_vertices(Vertices, Tree, Bindings) ->
    lists:foldl(
      fun(VertexLabel, Acc) ->
        Acc#{VertexLabel => render_vertex(VertexLabel, Tree, Bindings)}
      end,
      #{},
      Vertices
    ).

render_vertex(VertexLabel, Tree, Bindings) ->
    Vertices = maps:get(vertices, Tree),
    Vertex = maps:get(VertexLabel, Vertices),
    do_render_vertex(VertexLabel, Vertex, Tree, Bindings).

do_render_vertex(VertexLabel, Vertex, Tree, Bindings) ->
    Statics = maps:get(statics, Vertex),
    Dynamics = maps:get(dynamics, Vertex),
    render(VertexLabel, Statics, Dynamics, Tree, Bindings).

render(VertexLabel, Statics, Dynamics0, Tree, Bindings) ->
    Dynamics = render_dynamics(Dynamics0, Tree, Bindings),
    [Statics, Dynamics].
    % zip(Statics, Dynamics).

zip(Statics, Dynamics) ->
    do_zip(Statics, Dynamics, []).

do_zip([S | Statics], [D | Dynamics], Acc) ->
    do_zip(Statics, Dynamics, [D, S | Acc]);
do_zip([S | Statics], [], Acc) ->
    do_zip(Statics, [], [S | Acc]);
do_zip([], [], Acc) ->
    Acc;
do_zip([], Dynamics, Acc) ->
    [Dynamics | Acc].

render_dynamics(Dynamics, Tree, Bindings) ->
    lists:map(fun(Dynamic) -> render_dynamic(Dynamic, Tree, Bindings) end, Dynamics).

render_dynamic(#{ast := AST}, Tree, Bindings) ->
    {value, IOList, _} = erl_eval:exprs(AST, #{'Tree' => Tree, 'Bindings' => Bindings}),
    IOList;
render_dynamic(#{vertex := VertexLabel}, Tree, Bindings) ->
    render_vertex(VertexLabel, Tree, Bindings).
