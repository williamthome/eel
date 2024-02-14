-module(eel_compiler).

-export([compile/1, compile/2]).

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
       , metadata
       }).

%%======================================================================
%% API functions
%%======================================================================

compile({Root, Tree}) ->
    compile(Root, Tree).

compile(VertexLabel, Tree) ->
    Vertex = eel_tree:fetch_vertex(VertexLabel, Tree),
    State0 = #state{
        parts = #{},
        vars = [],
        dynamics = [],
        index = 0,
        recursive = false,
        tree = Tree,
        metadata = #{}
    },
    State = do_fold_compile(Vertex, State0),
    eel_renderer:new_state(
        State#state.parts,
        State#state.vars,
        lists:reverse(State#state.dynamics),
        State#state.metadata
    ).

%%======================================================================
%% Internal functions
%%======================================================================

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
    case maps:find(Index, Parts) of
        {ok, Expr} ->
            State#state{
                parts = Parts#{
                    Index => lists:join($,, lists:reverse(Expr))
                }
            };
        error ->
            State
    end.

do_fold_compile_1(#master_vertex{}, #state{vertices = [Vertex | _]} = State) ->
    do_fold_compile(Vertex, State);
do_fold_compile_1(#slave_vertex{token = Token}, State) ->
    do_fold_compile_2(Token, State).

% TODO: Marker match must bem more generic.
%       The engine should be able to handle it.
do_fold_compile_2( #text_token{} = Token
                 , #state{recursive = false} = State ) ->
    Index = State#state.index,
    Parts = State#state.parts,
    Metadata = State#state.metadata,
    Static = Token#text_token.text,
    State#state{
        parts = Parts#{
            Index => Static
        },
        index = Index+1,
        metadata = Metadata#{
            Index => Token#text_token.metadata
        }
    };
do_fold_compile_2( #text_token{} = Token
                 , #state{recursive = true} = State ) ->
    Index = State#state.index,
    Parts = State#state.parts,
    Metadata = State#state.metadata,
    IndexParts = maps:get(Index, Parts, []),
    Static = Token#text_token.text,
    State#state{
        parts = Parts#{
            Index => [<<"<<\"", Static/binary, "\"/utf8>>">> | IndexParts]
        },
        metadata = Metadata#{
            Index => Token#text_token.metadata
        }
    };
do_fold_compile_2( #expr_token{marker = #marker{compile_as = expr}} = Token
                 , #state{recursive = false} = State ) ->
    Index = State#state.index,
    Parts = State#state.parts,
    Metadata = State#state.metadata,
    {ok, AST} = expr_ast(Token#expr_token.expr),
    Vars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    State#state{
        parts = Parts#{
            Index => AST
        },
        index = Index+1,
        vars = lists:merge(State#state.vars, Vars),
        dynamics = [Index | State#state.dynamics],
        metadata = Metadata#{
            Index => Token#expr_token.metadata
        }
    };
do_fold_compile_2( #expr_token{marker = #marker{compile_as = expr}} = Token
                 , #state{recursive = true} = State ) ->
    Index = State#state.index,
    Parts = State#state.parts,
    Metadata = State#state.metadata,
    IndexParts = maps:get(Index, Parts, []),
    Vars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    State#state{
        parts = Parts#{
            Index => [Token#expr_token.expr | IndexParts]
        },
        vars = lists:merge(State#state.vars, Vars),
        metadata = Metadata#{
            Index => Token#expr_token.metadata
        }
    };
do_fold_compile_2( #expr_token{marker = #marker{compile_as = expr_start}} = Token
                 , #state{recursive = false} = State0 ) ->
    {Expr, State} = do_fold_compile_3( tl(State0#state.vertices)
                                     , Token#expr_token.expr, State0 ),
    Index = State#state.index,
    Parts = State#state.parts,
    Metadata = State#state.metadata,
    {ok, AST} = expr_ast(Expr),
    Vars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    {halt, State#state{
        parts = Parts#{
            Index => AST
        },
        index = Index+1,
        vars = lists:merge(State#state.vars, Vars),
        dynamics = [Index | State#state.dynamics],
        metadata = Metadata#{
            Index => Token#expr_token.metadata
        }
    }};
do_fold_compile_2( #expr_token{marker = #marker{compile_as = expr_start}} = Token
                 , #state{recursive = true} = State0 ) ->
    {Expr, State} = do_fold_compile_3( tl(State0#state.vertices)
                                     , Token#expr_token.expr, State0 ),
    Index = State#state.index,
    Parts = State#state.parts,
    Metadata = State#state.metadata,
    Vars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    {halt, State#state{
        parts = Parts#{
            Index => Expr
        },
        index = Index+1,
        vars = lists:merge(State#state.vars, Vars),
        metadata = Metadata#{
            Index => Token#expr_token.metadata
        }
    }}.

do_fold_compile_3([Vertex | T], Expr0, State0) ->
    Token = eel_tree:get_vertex_metadata(Vertex),
    {Expr, State} = do_fold_compile_4(Token, Vertex, Expr0, State0),
    do_fold_compile_3(T, Expr, State);
do_fold_compile_3([], Expr, State) ->
    {Expr, State}.

do_fold_compile_4(#master_vertex{}, Vertex, Expr0, State0) ->
    StateTmp = do_fold_compile(Vertex, State0#state{parts = #{}, recursive = true}),
    Expr1 = maps:values(StateTmp#state.parts),
    Expr = [Expr0, $\s, $[, Expr1, $], $\s],
    State = State0#state{
        vars = StateTmp#state.vars,
        dynamics = StateTmp#state.dynamics
    },
    {Expr, State};
do_fold_compile_4(#slave_vertex{token = #expr_token{} = Token}, _Vertex, Expr0, State0) ->
    Expr = [Expr0, $\s, Token#expr_token.expr],
    Index = State0#state.index,
    ExprVars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    State = State0#state{
        vars = State0#state.vars ++ ExprVars
    },
    {Expr, State}.

%%======================================================================
%% Internal functions
%%======================================================================

expr_ast(Bin) ->
    String = binary_to_list(iolist_to_binary(normalize_expr(Bin))),
    case erl_scan:string(String) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, AST} ->
                    {ok, AST};
                {error, Reason} ->
                    {error, {parse, String, Reason}}
            end;
        {error, ErrorInfo, ErrorLocation} ->
            {error, {scan, {String, ErrorInfo, ErrorLocation}}}
    end.

normalize_expr(Expr) ->
    [Expr, $.].

%%======================================================================
%% Tests
%%======================================================================

-ifdef(TEST).

compile_test() ->
    Expected = {render_state,
    #{0 => <<"<html><head><title>">>,
      1 =>
       [{call,1,
         {remote,1,{atom,1,maps},{atom,1,get}},
         [{atom,1,title},{var,1,'Assigns'}]}],
      2 => <<"</title></head><body><ul>">>,
      3 =>
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
           [{atom,1,items},{var,1,'Assigns'}]}]}],
      4 => <<"</ul></body></html>">>},
    [{item_prefix,3},{title,1},{items,3}],
    [3,1],
    #{0 => undefined,1 => undefined,2 => undefined,
      3 => undefined,4 => undefined},
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
    Tokens = eel_tokenizer:tokenize(Bin),
    Tree = eel_structurer:tree(Tokens),
    Result = compile(Tree),
    ?assertEqual(Expected, Result).

-endif.
