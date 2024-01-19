-module(eel_structurer).

-export([tree/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("eel.hrl").

%%======================================================================
%% API functions
%%======================================================================

tree(Tokens) ->
    Tree0 = eel_tree:new(),
    {VRoot, Tree1} = eel_tree:add_vertex(Tree0, #{metadata => #master_vertex{}}),
    MVertices = [eel_tree:get_vertex_label(VRoot)],
    State = #tree_state{master_vertices = MVertices},
    Tree = eel_tree:set_metadata(State, Tree1),
    do_tree(Tokens, VRoot, Tree).

%%======================================================================
%% Internal functions
%%======================================================================

do_tree(Tokens, VParent, Tree) ->
    lists:foldl(fun
        (#text_token{} = Token, {VParent0, Tree0}) ->
            {_SVertex, Tree1} = add_slave_vertex(Token, VParent0, Tree0),
            {VParent0, Tree1};
        (#expr_token{marker = Marker} = Token, {VParent0, Tree0}) ->
            resolve_tree_behaviors(
                Marker#marker.tree_behaviors,
                Token,
                VParent0,
                Tree0
            );
        (List, {VParent0, Tree0}) when is_list(List) ->
            do_tree(List, VParent0, Tree0)
    end, {VParent, Tree}, Tokens).

resolve_tree_behaviors(Behaviors, Token, VParent0, Tree0) ->
    lists:foldl(fun(Behavior, {VParent, Tree}) ->
        resolve_tree_behavior(Behavior, Token, VParent, Tree)
    end, {VParent0, Tree0}, Behaviors).

resolve_tree_behavior(push_token, Token, VParent0, Tree0) ->
    {_SVertex, Tree} = add_slave_vertex(Token, VParent0, Tree0),
    {VParent0, Tree};
resolve_tree_behavior(add_vertex, _Token, VParent0, Tree0) ->
    {MVertex, Tree} = add_master_vertex(VParent0, Tree0),
    {MVertex, Tree};
resolve_tree_behavior(fetch_vertex_parent, _Token, VParent0, Tree) ->
    VParent = eel_tree:fetch_vertex_parent(VParent0, Tree),
    {VParent, Tree};
resolve_tree_behavior(ignore_token, _Token, VParent, Tree) ->
    {VParent, Tree};
resolve_tree_behavior(Fun, Token, VParent, Tree) when is_function(Fun, 3) ->
    Fun(Token, VParent, Tree).

add_master_vertex(PVertex, Tree0) ->
    Opts = #{metadata => #master_vertex{}},
    {MVertex, Tree1} = eel_tree:add_vertex(Tree0, Opts),
    Tree2 = eel_tree:add_edge(PVertex, MVertex, Tree1),
    State = eel_tree:get_metadata(Tree2),
    MVertices = [eel_tree:get_vertex_label(MVertex) | State#tree_state.master_vertices],
    Tree = eel_tree:set_metadata(
        State#tree_state{master_vertices = MVertices},
        Tree2
    ),
    {MVertex, Tree}.

add_slave_vertex(Token, MVertex, Tree0) ->
    Opts = #{metadata => #slave_vertex{token = Token}},
    {SVertex, Tree1} = eel_tree:add_vertex(Tree0, Opts),
    Tree = eel_tree:add_edge(MVertex, SVertex, Tree1),
    {SVertex, Tree}.

%%======================================================================
%% Tests
%%======================================================================

-ifdef(TEST).

tree_test() ->
    Expected = {0,
    {tree,13,
     #{0 =>
        {vertex,undefined,0,
         [12,4,3,2,1],
         true,false,
         {master_vertex,undefined}},
       1 =>
        {vertex,0,1,[],false,true,
         {slave_vertex,{text_token,<<"<html><head><title>">>}}},
       2 =>
        {vertex,0,2,[],false,true,
         {slave_vertex,
          {expr_token,<<"(maps:get(title, Bindings))">>,
           eel_smart_engine,
           {marker,expr,<<"<%=\\s+">>,<<"\\s+.%>">>,[],[],
            [push_token]},
           [title]}}},
       3 =>
        {vertex,0,3,[],false,true,
         {slave_vertex,
          {text_token,<<"</title></head><body><ul>">>}}},
       4 =>
        {vertex,0,4,
         [11,6,5],
         false,false,
         {master_vertex,undefined}},
       5 =>
        {vertex,4,5,[],false,true,
         {slave_vertex,
          {expr_token,<<"lists:map(fun(Item) ->">>,
           eel_smart_engine,
           {marker,expr_start,<<"<%=\\s+">>,<<"\\s+%>">>,[],[],
            [add_vertex,push_token,add_vertex]},
           []}}},
       6 =>
        {vertex,4,6,
         [10,9,8,7],
         false,false,
         {master_vertex,undefined}},
       7 =>
        {vertex,6,7,[],false,true,
         {slave_vertex,{text_token,<<"<li>">>}}},
       8 =>
        {vertex,6,8,[],false,true,
         {slave_vertex,
          {expr_token,<<"(maps:get(item_prefix, Bindings))">>,
           eel_smart_engine,
           {marker,expr,<<"<%=\\s+">>,<<"\\s+.%>">>,[],[],
            [push_token]},
           [item_prefix]}}},
       9 =>
        {vertex,6,9,[],false,true,
         {slave_vertex,
          {expr_token,<<"integer_to_binary(Item)">>,
           eel_smart_engine,
           {marker,expr,<<"<%=\\s+">>,<<"\\s+.%>">>,[],[],
            [push_token]},
           []}}},
       10 =>
        {vertex,6,10,[],false,true,
         {slave_vertex,{text_token,<<"</li>">>}}},
       11 =>
        {vertex,4,11,[],false,true,
         {slave_vertex,
          {expr_token,<<"end, (maps:get(items, Bindings)))">>,
           eel_smart_engine,
           {marker,expr_end,<<"<%\\s+">>,<<"\\s+.%>">>,[],[],
            [fetch_vertex_parent,push_token,
             fetch_vertex_parent]},
           [items]}}},
       12 =>
        {vertex,0,12,[],false,true,
         {slave_vertex,{text_token,<<"</ul></body></html>">>}}}},
     0,
     {tree_state,[6,4,0]}}},

    Bin = <<
        "<html>"
        "<head>"
            "<title><%= @title .%></title>"
        "</head>"
        "<body>"
            "<ul>"
            "<%= lists:map(fun(Item) -> %>"
                "<%% TODO: Items to binary. %%>"
                "<li><%= @item_prefix .%><%= integer_to_binary(Item) .%></li>"
            "<% end, @items) .%>"
            "</ul>"
        "</body>"
        "</html>"
    >>,
    Tokens = eel_tokenizer:tokenize(Bin),
    Result = tree(Tokens),
    ?assertEqual(Expected, Result).

-endif.
