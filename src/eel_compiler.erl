%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023-2024 William Fank Thomé
%% @doc Compiler.

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
-module(eel_compiler).

%% API
-export([ compile/1, compile/2, compile/3 ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("eel.hrl").

-record( state,
       { parts
       , vars
       , dynamics
       , index
       , recursive
       , tree
       , vertices
       , records
       }).

%%%=====================================================================
%%% API
%%%=====================================================================

compile({Root, Tree}) ->
    compile(Root, Tree).

compile({Root, Tree}, Opts) ->
    compile(Root, Tree, Opts);
compile(VertexLabel, Tree) ->
    compile(VertexLabel, Tree, #{}).

compile(VertexLabel, Tree, Opts) ->
    Vertex = eel_tree:fetch_vertex(VertexLabel, Tree),
    State0 = #state{
        parts = [],
        vars = [],
        dynamics = [],
        index = 0,
        recursive = false,
        tree = Tree,
        records = maps:get(records, Opts, [])
    },
    State = do_fold_compile(Vertex, State0),
    eel_renderer:new_state(
        State#state.parts,
        State#state.vars,
        lists:reverse(State#state.dynamics)
    ).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

do_fold_compile(Vertex, State) ->
    Children = lists:reverse(eel_tree:fetch_vertex_children(Vertex, State#state.tree)),
    do_fold_compile_0(Children, State).

do_fold_compile_0([Vertex | T] = Vertices, State) ->
    Metadata = eel_tree:get_vertex_metadata(Vertex),
    case do_fold_compile_1(Metadata, State#state{vertices = Vertices}) of
        {halt, State1} ->
            State1;
        State1 ->
            do_fold_compile_0(T, State1)
        end;
do_fold_compile_0([], State) ->
    Index = State#state.index,
    Parts = State#state.parts,
    case proplists:lookup(Index, Parts) of
        {Index, Expr} ->
            State#state{
                parts = lists:keystore(Index, 1, Parts, {Index, lists:join($,, lists:reverse(Expr))})
            };
        none ->
            State
    end.

do_fold_compile_1(#master_vertex{}, #state{vertices = [Vertex | _]} = State) ->
    do_fold_compile(Vertex, State);
do_fold_compile_1(#slave_vertex{token = Token}, State) ->
    do_fold_compile_2(Token, State).

do_fold_compile_2( #text_token{} = Token
                 , #state{recursive = false} = State ) ->
    Index = State#state.index,
    Parts = State#state.parts,
    Static = Token#text_token.text,
    State#state{
        parts = lists:keystore(Index, 1, Parts, {Index, Static}),
        index = Index+1
    };
do_fold_compile_2( #text_token{} = Token
                 , #state{recursive = true} = State ) ->
    Index = State#state.index,
    Parts = State#state.parts,
    IndexParts = proplists:get_value(Index, Parts, []),
    Static = Token#text_token.text,
    State#state{
        parts = lists:keystore(Index, 1, Parts,
            {Index, [<<"<<\"", Static/binary, "\"/utf8>>">> | IndexParts]}
        )
    };
do_fold_compile_2( #expr_token{marker = #marker{compile_as = expr}} = Token
                 , #state{recursive = false} = State ) ->
    Index = State#state.index,
    Parts = State#state.parts,
    {ok, AST} = expr_ast(Token#expr_token.expr, State#state.records),
    Vars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    State#state{
        parts = lists:keystore(Index, 1, Parts, {Index, AST}),
        index = Index+1,
        vars = lists:merge(State#state.vars, Vars),
        dynamics = [Index | State#state.dynamics]
    };
do_fold_compile_2( #expr_token{marker = #marker{compile_as = expr}} = Token
                 , #state{recursive = true} = State ) ->
    Index = State#state.index,
    Parts = State#state.parts,
    IndexParts = proplists:get_value(Index, Parts, []),
    Vars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    State#state{
        parts = lists:keystore(Index, 1, Parts, {Index, [Token#expr_token.expr | IndexParts]}),
        vars = lists:merge(State#state.vars, Vars)
    };
do_fold_compile_2( #expr_token{marker = #marker{compile_as = expr_start}} = Token
                 , #state{recursive = false} = State0 ) ->
    {Expr, State} = do_fold_compile_3( tl(State0#state.vertices)
                                     , Token#expr_token.expr, State0 ),
    Index = State#state.index,
    Parts = State#state.parts,
    {ok, AST} = expr_ast(Expr, State#state.records),
    Vars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    {halt, State#state{
        parts = lists:keystore(Index, 1, Parts, {Index, AST}),
        index = Index+1,
        vars = lists:merge(State#state.vars, Vars),
        dynamics = [Index | State#state.dynamics]
    }};
do_fold_compile_2( #expr_token{marker = #marker{compile_as = expr_start}} = Token
                 , #state{recursive = true} = State0 ) ->
    {Expr, State} = do_fold_compile_3( tl(State0#state.vertices)
                                     , Token#expr_token.expr, State0 ),
    Index = State#state.index,
    Parts = State#state.parts,
    IndexParts = proplists:get_value(State0#state.index, State0#state.parts, []),
    Vars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    {halt, State#state{
        parts = lists:keystore(Index, 1, Parts, {Index, lists:join($,, [IndexParts | Expr])}),
        index = Index+1,
        vars = lists:merge(State#state.vars, Vars)
    }}.

do_fold_compile_3([Vertex | T], Expr0, State0) ->
    Token = eel_tree:get_vertex_metadata(Vertex),
    {Expr, State} = do_fold_compile_4(Token, Vertex, Expr0, State0),
    do_fold_compile_3(T, Expr, State);
do_fold_compile_3([], Expr, State) ->
    {Expr, State}.

do_fold_compile_4(#master_vertex{}, Vertex, Expr0, State0) ->
    StateTmp = do_fold_compile(Vertex, State0#state{parts = [], recursive = true}),
    Expr1 = lists:map(fun({_, V}) -> V end, StateTmp#state.parts),
    Expr = case State0#state.recursive of
        true ->
            [[Expr0, $\s, $[, Expr1, $], $\s]];
        false ->
            [Expr0, $\s, $[, lists:join($,, Expr1), $], $\s]
    end,
    Index = State0#state.index,
    Parts = State0#state.parts,
    State = State0#state{
        parts = lists:keystore(Index, 1, Parts, {Index, Expr}),
        vars = StateTmp#state.vars,
        dynamics = lists:reverse(StateTmp#state.dynamics)
    },
    {Expr, State};
do_fold_compile_4(#slave_vertex{token = #expr_token{} = Token}, _Vertex, Expr0, State0) ->
    Expr = [[Expr0, $\s, Token#expr_token.expr]],
    Index = State0#state.index,
    Parts = State0#state.parts,
    ExprVars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    State = State0#state{
        parts = lists:keystore(Index, 1, Parts, {Index, Expr}),
        vars = State0#state.vars ++ ExprVars
    },
    {Expr, State}.

expr_ast(Expr0, Records) ->
    Expr = normalize_expr(Expr0),
    case erl_scan:string(Expr) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, AST} ->
                    {ok, expand_records(Records, AST)};
                {error, Reason} ->
                    {error, {parse, Expr, Reason}}
            end;
        {error, ErrorInfo, ErrorLocation} ->
            {error, {scan, {Expr, ErrorInfo, ErrorLocation}}}
    end.

% @see https://erlangforums.com/t/how-to-evaluate-expressions-containing-record-reference/3273/6?u=williamthome
expand_records(Records, AST) ->
    Forms = Records ++ wrap_ast(AST),
    Fun = lists:last(erl_expand_records:module(Forms, [])),
    unwrap_fun(Fun).

% TODO: Use erl_syntax.
wrap_ast(Expr) ->
    [{function, 1, {atom, 1, foo}, 1, [{clause, 1, [], [], Expr}]}].

% TODO: Use erl_syntax.
unwrap_fun({function, 1, {atom, 1, foo}, 1, [{clause, 1, [], [], Expr}]}) ->
    Expr.

normalize_expr(Expr0) ->
    Expr = binary_to_list(string:trim(iolist_to_binary(Expr0))),
    case lists:last(Expr) =:= $. of
        true ->
            Expr;
        false ->
            Expr ++ [$.]
    end.

%%%=====================================================================
%%% Test
%%%=====================================================================

-ifdef(TEST).

compile_test() ->
    Expected = {render_state,
    [{0,<<"<html><head><title>">>},
     {1,
      [{call,1,
        {remote,1,{atom,1,maps},{atom,1,get}},
        [{atom,1,title},{var,1,'Assigns'}]}]},
     {2,<<"</title></head><body><ul>">>},
     {3,
      [{call,1,
        {remote,1,{atom,1,lists},{atom,1,map}},
        [{'fun',1,
          {clauses,
           [{clause,1,
             [{var,1,'Item'}],
             [],
             [{cons,1,
               {bin,1,
                [{bin_element,1,
                  {string,1,"<li>"},
                  default,
                  [utf8]}]},
               {cons,1,
                {call,1,
                 {remote,1,{atom,1,maps},{atom,1,get}},
                 [{atom,1,item_prefix},{var,1,'Assigns'}]},
                {cons,1,
                 {var,1,'Item'},
                 {cons,1,
                  {bin,1,
                   [{bin_element,1,
                     {string,1,"</li>"},
                     default,
                     [utf8]}]},
                  {nil,1}}}}}]}]}},
         {call,1,
          {remote,1,{atom,1,maps},{atom,1,get}},
          [{atom,1,items},{var,1,'Assigns'}]}]}]},
     {4,<<"</ul></body></html>">>}],
    [{item_prefix,3},{title,1},{items,3}],
    [1,3],
    undefined},

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
    TokenizeState = eel_tokenizer:tokenize(Bin),
    Tokens = eel_tokenizer:get_tokens(TokenizeState),
    Tree = eel_structurer:tree(Tokens),
    Result = compile(Tree),
    ?assertEqual(Expected, Result).

-endif.
