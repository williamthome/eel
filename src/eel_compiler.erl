-module(eel_compiler).

-export([compile/1, compile/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("eel.hrl").

% TODO
-record( state,
       { parts
       , vars
       , dynamics
       , index
       , recursive
       , tree
       , vertices
       , converter
       }).

% TODO: Check if we should use 'eel_converter' or keep the
%       'undefined' as the value converter.
-define(CONVERTER, undefined).

%%======================================================================
%% API functions
%%======================================================================

compile(Payload) ->
    compile(Payload, #{}).

compile({Root, Tree}, Opts) ->
    State = fold_compile(Root, Tree, Opts),
    #{
        parts => State#state.parts,
        vars => State#state.vars,
        dynamics => State#state.dynamics
    }.

%%======================================================================
%% Internal functions
%%======================================================================

default_converter() ->
    ?CONVERTER.

fold_compile(VertexLabel, Tree, Opts) ->
    Vertex = eel_tree:fetch_vertex(VertexLabel, Tree),
    State = #state{
        parts = #{},
        vars = [],
        dynamics = [],
        index = 0,
        recursive = false,
        tree = Tree,
        converter = maps:get(converter, Opts, default_converter())
    },
    do_fold_compile(Vertex, State).

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

do_fold_compile_2( #text_token{} = Token
                 , #state{recursive = false} = State ) ->
    Index = State#state.index,
    Parts = State#state.parts,
    Static = Token#text_token.text,
    State#state{
        parts = Parts#{
            Index => Static
        },
        index = Index+1
    };
do_fold_compile_2( #text_token{} = Token
                 , #state{recursive = true} = State ) ->
    Index = State#state.index,
    Parts = State#state.parts,
    IndexParts = maps:get(Index, Parts, []),
    Static = Token#text_token.text,
    State#state{
        parts = Parts#{
            Index => [<<"<<\"", Static/binary, "\"/utf8>>">> | IndexParts]
        }
    };
do_fold_compile_2( #expr_token{marker = #marker{id = expr}} = Token
                 , #state{recursive = false} = State ) ->
    Index = State#state.index,
    Parts = State#state.parts,
    {ok, AST} = binary_to_ast(Token#expr_token.expr, State#state.converter),
    Vars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    State#state{
        parts = Parts#{
            Index => AST
        },
        index = Index+1,
        vars = lists:merge(State#state.vars, Vars),
        dynamics = [Index | State#state.dynamics]
    };
do_fold_compile_2( #expr_token{marker = #marker{id = expr}} = Token
                 , #state{recursive = true} = State ) ->
    Index = State#state.index,
    Parts = State#state.parts,
    IndexParts = maps:get(Index, Parts, []),
    Vars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    State#state{
        parts = Parts#{
            Index => [Token#expr_token.expr | IndexParts]
        },
        vars = lists:merge(State#state.vars, Vars)
    };
do_fold_compile_2( #expr_token{marker = #marker{id = expr_start}} = Token
                 , #state{recursive = false} = State0 ) ->
    {Expr, State} = do_fold_compile_3( tl(State0#state.vertices)
                                     , Token#expr_token.expr, State0 ),
    Index = State#state.index,
    Parts = State#state.parts,
    {ok, AST} = binary_to_ast(Expr, State#state.converter),
    Vars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    {halt, State#state{
        parts = Parts#{
            Index => AST
        },
        index = Index+1,
        vars = lists:merge(State#state.vars, Vars),
        dynamics = [Index | State#state.dynamics]
    }};
do_fold_compile_2( #expr_token{marker = #marker{id = expr_start}} = Token
                 , #state{recursive = true} = State0 ) ->
    {Expr, State} = do_fold_compile_3( tl(State0#state.vertices)
                                     , Token#expr_token.expr, State0 ),
    Index = State#state.index,
    Parts = State#state.parts,
    Vars = lists:map(fun(Var) -> {Var, Index} end, Token#expr_token.vars),
    {halt, State#state{
        parts = Parts#{
            Index => Expr
        },
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
    StateTmp = do_fold_compile(Vertex, State0#state{parts = #{}, recursive = true}),
    Expr1 = maps:values(StateTmp#state.parts),
    Expr = [Expr0, 32, $[, Expr1, $], 32],
    State = State0#state{
        vars = StateTmp#state.vars,
        dynamics = StateTmp#state.dynamics
    },
    {Expr, State};
do_fold_compile_4(#slave_vertex{token = #expr_token{expr = Expr1}}, _, Expr0, State) ->
    Expr = [Expr0, 32, Expr1],
    {Expr, State}.

%%======================================================================
%% Internal functions
%%======================================================================

binary_to_ast(Bin, Converter) ->
    String = binary_to_list(iolist_to_binary(normalize_bin(Bin, Converter))),
    case erl_scan:string(String) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, AST} ->
                    {ok, AST};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, ErrorInfo, ErrorLocation} ->
            {error, {ErrorInfo, ErrorLocation}}
    end.

normalize_bin(Expr, undefined) ->
    [Expr, $.];
normalize_bin(Expr, Converter)
  when is_binary(Converter); is_list(Converter) ->
    [Converter, ":to_string(", Expr, ")."];
normalize_bin(Expr, Converter) when is_atom(Converter) ->
    normalize_bin(Expr, atom_to_binary(Converter)).

%%======================================================================
%% Tests
%%======================================================================

-ifdef(TEST).

compile_test() ->
    Expected = #{parts =>
        #{0 => <<"<html><head><title>">>,
          1 =>
           [{call,1,
             {remote,1,{atom,1,maps},{atom,1,get}},
             [{atom,1,title},{var,1,'Bindings'}]}],
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
                      [{atom,1,item_prefix},{var,1,'Bindings'}]},
                     {cons,1,
                      {call,1,
                       {atom,1,integer_to_binary},
                       [{var,1,'Item'}]},
                      {cons,1,
                       {bin,1,
                        [{bin_element,1,
                          {string,1,"</li>"},
                          default,
                          [utf8]}]},
                       {nil,1}}}}}]}]}},
              {call,1,
               {remote,1,{atom,1,maps},{atom,1,get}},
               [{atom,1,items},{var,1,'Bindings'}]}]}],
          4 => <<"</ul></body></html>">>},
       dynamics => [3,1],
       vars => [{item_prefix,3},{title,1}]},

    Bin = <<
        "<html>"
        "<head>"
            "<title><%= @title .%></title>"
        "</head>"
        "<body>"
            "<ul>"
            "<%= lists:map(fun(Item) -> %>"
                "<li><%= @item_prefix .%><%= integer_to_binary(Item) .%></li>"
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
