%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023-2024 William Fank Thomé
%% @doc Structurer.

%% Copyright 2023-2024 William Fank Thomé
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(eel_structurer).

%% API
-export([ tree/1 ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("eel.hrl").

%%%=====================================================================
%%% API
%%%=====================================================================

tree(Tokens) ->
    Tree0 = eel_tree:new(),
    {VRoot, Tree1} = eel_tree:add_vertex(Tree0, #{metadata => #master_vertex{}}),
    MVertices = [eel_tree:get_vertex_label(VRoot)],
    State = #tree_state{master_vertices = MVertices},
    Tree = eel_tree:set_metadata(State, Tree1),
    do_tree(Tokens, VRoot, Tree).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

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

%%%=====================================================================
%%% Test
%%%=====================================================================

-ifdef(TEST).

tree_test() ->
    Expected = {0,
    {tree,13,
     [{0,
       {vertex,undefined,0,
        [12,4,3,2,1],
        true,false,
        {master_vertex,undefined}}},
      {1,
       {vertex,0,1,[],false,true,
        {slave_vertex,
         {text_token,<<"<html><head><title>">>,undefined}}}},
      {2,
       {vertex,0,2,[],false,true,
        {slave_vertex,
         {expr_token,<<"(maps:get(title, Assigns))">>,
          eel_smart_engine,
          {marker,expr,
           {re_pattern,0,0,0,
            <<69,82,67,80,80,0,0,0,0,0,0,0,81,0,0,0,255,255,255,
              255,255,255,255,255,60,0,61,0,0,0,0,0,0,0,64,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,131,0,12,29,60,29,37,29,61,87,9,25,120,0,12,0>>},
           {re_pattern,0,0,0,
            <<69,82,67,80,79,0,0,0,0,0,0,0,65,0,0,0,255,255,255,
              255,255,255,255,255,0,0,62,0,0,0,0,0,0,0,64,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,131,0,11,87,9,12,29,37,29,62,25,120,0,11,0>>},
           [],[],
           [push_token],
           expr},
          [title],
          undefined}}}},
      {3,
       {vertex,0,3,[],false,true,
        {slave_vertex,
         {text_token,<<"</title></head><body><ul>">>,
          undefined}}}},
      {4,
       {vertex,0,4,
        [11,6,5],
        false,false,
        {master_vertex,undefined}}},
      {5,
       {vertex,4,5,[],false,true,
        {slave_vertex,
         {expr_token,<<"lists:map(fun(Item) ->">>,
          eel_smart_engine,
          {marker,expr_start,
           {re_pattern,0,0,0,
            <<69,82,67,80,80,0,0,0,0,0,0,0,81,0,0,0,255,255,255,
              255,255,255,255,255,60,0,61,0,0,0,0,0,0,0,64,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,131,0,12,29,60,29,37,29,61,87,9,25,120,0,12,0>>},
           {re_pattern,0,0,0,
            <<69,82,67,80,78,0,0,0,0,0,0,0,65,0,0,0,255,255,255,
              255,255,255,255,255,0,0,62,0,0,0,0,0,0,0,64,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,131,0,10,95,9,29,37,29,62,25,120,0,10,0>>},
           [],[],
           [add_vertex,push_token,add_vertex],
           expr_start},
          [],undefined}}}},
      {6,
       {vertex,4,6,
        [10,9,8,7],
        false,false,
        {master_vertex,undefined}}},
      {7,
       {vertex,6,7,[],false,true,
        {slave_vertex,{text_token,<<"<li>">>,undefined}}}},
      {8,
       {vertex,6,8,[],false,true,
        {slave_vertex,
         {expr_token,<<"(maps:get(item_prefix, Assigns))">>,
          eel_smart_engine,
          {marker,expr,
           {re_pattern,0,0,0,
            <<69,82,67,80,80,0,0,0,0,0,0,0,81,0,0,0,255,255,255,
              255,255,255,255,255,60,0,61,0,0,0,0,0,0,0,64,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,131,0,12,29,60,29,37,29,61,87,9,25,120,0,12,0>>},
           {re_pattern,0,0,0,
            <<69,82,67,80,79,0,0,0,0,0,0,0,65,0,0,0,255,255,255,
              255,255,255,255,255,0,0,62,0,0,0,0,0,0,0,64,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,131,0,11,87,9,12,29,37,29,62,25,120,0,11,0>>},
           [],[],
           [push_token],
           expr},
          [item_prefix],
          undefined}}}},
      {9,
       {vertex,6,9,[],false,true,
        {slave_vertex,
         {expr_token,<<"Item">>,eel_smart_engine,
          {marker,expr,
           {re_pattern,0,0,0,
            <<69,82,67,80,80,0,0,0,0,0,0,0,81,0,0,0,255,255,255,
              255,255,255,255,255,60,0,61,0,0,0,0,0,0,0,64,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,131,0,12,29,60,29,37,29,61,87,9,25,120,0,12,0>>},
           {re_pattern,0,0,0,
            <<69,82,67,80,79,0,0,0,0,0,0,0,65,0,0,0,255,255,255,
              255,255,255,255,255,0,0,62,0,0,0,0,0,0,0,64,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,131,0,11,87,9,12,29,37,29,62,25,120,0,11,0>>},
           [],[],
           [push_token],
           expr},
          [],undefined}}}},
      {10,
       {vertex,6,10,[],false,true,
        {slave_vertex,{text_token,<<"</li>">>,undefined}}}},
      {11,
       {vertex,4,11,[],false,true,
        {slave_vertex,
         {expr_token,<<"end, (maps:get(items, Assigns)))">>,
          eel_smart_engine,
          {marker,expr_end,
           {re_pattern,0,0,0,
            <<69,82,67,80,78,0,0,0,0,0,0,0,81,0,0,0,255,255,255,
              255,255,255,255,255,60,0,37,0,0,0,0,0,0,0,64,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,131,0,10,29,60,29,37,87,9,25,120,0,10,0>>},
           {re_pattern,0,0,0,
            <<69,82,67,80,79,0,0,0,0,0,0,0,65,0,0,0,255,255,255,
              255,255,255,255,255,0,0,62,0,0,0,0,0,0,0,64,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,131,0,11,87,9,12,29,37,29,62,25,120,0,11,0>>},
           [],[],
           [fetch_vertex_parent,push_token,fetch_vertex_parent],
           undefined},
          [items],
          undefined}}}},
      {12,
       {vertex,0,12,[],false,true,
        {slave_vertex,
         {text_token,<<"</ul></body></html>">>,undefined}}}}],
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
                "<li><%= @item_prefix .%><%= Item .%></li>"
            "<% end, @items) .%>"
            "</ul>"
        "</body>"
        "</html>"
    >>,
    {Tokens, _State} = eel_tokenizer:tokenize(Bin),
    Result = tree(Tokens),
    ?assertEqual(Expected, Result).

-endif.
