-module(v2_eel_tokenizer).

-export([tokenize/1, tokenize/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("v2_eel.hrl").

-record(state, { engines   :: [engine()]
               , buffer    :: binary()
               , text_acc  :: binary()
               , tokens    :: [token()]
               , converter :: module()
               }).

-define(SMART_ENGINE, v2_eel_smart_engine).
-define(CONVERTER, v2_eel_converter).

%%%=============================================================================
%%% API functions
%%%=============================================================================

tokenize(Bin) ->
    tokenize(Bin, #{}).

tokenize(Bin, Opts) when is_binary(Bin), is_map(Opts) ->
    State = #state{
        engines = maps:get(engines, Opts, default_engines()),
        buffer = <<>>,
        text_acc = <<>>,
        tokens = [],
        converter = atom_to_binary(maps:get(converter, Opts, default_converter()))
    },
    do_tokenize(Bin, State).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

default_engines() ->
    [?SMART_ENGINE].

default_converter() ->
    ?CONVERTER.

do_tokenize(<<H, T/binary>>, State0) ->
    State = State0#state{
        buffer = <<(State0#state.buffer)/binary, H>>,
        text_acc = <<(State0#state.text_acc)/binary, H>>
    },
    case handle_expr_start(State#state.engines, State#state.text_acc) of
        {ok, {Engine, Markers}} ->
            case handle_expr_end(T, Markers, <<>>) of
                {ok, {Marker, Text, Expr, Rest}} ->
                    case handle_text(State#state.engines, Text, State#state.converter) of
                        {ok, FirstToken} ->
                            case handle_expr(Engine, Marker, Expr, State#state.converter) of
                                {ok, SecondToken} ->
                                    do_tokenize(Rest, State#state{
                                        buffer = <<(State#state.buffer)/binary, Expr/binary>>,
                                        tokens = [SecondToken, FirstToken | State#state.tokens],
                                        text_acc = <<>>
                                    });
                                ignore ->
                                    do_tokenize(Rest, State#state{
                                        buffer = <<(State#state.buffer)/binary, Expr/binary>>,
                                        tokens = [FirstToken | State#state.tokens],
                                        text_acc = <<>>
                                    });
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        ignore ->
                            case handle_expr(Engine, Marker, Expr, State#state.converter) of
                                {ok, SecondToken} ->
                                    do_tokenize(Rest, State#state{
                                        buffer = <<(State#state.buffer)/binary, Expr/binary>>,
                                        tokens = [SecondToken | State#state.tokens],
                                        text_acc = <<>>
                                    });
                                ignore ->
                                    do_tokenize(Rest, State#state{
                                        text_acc = <<>>
                                    });
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                none ->
                    % TODO: Check if should just skip when no end marker found.
                    %       e.g: do_tokenize(T, State)
                    error({noendmarker, #{
                        engine => Engine,
                        markers_candidate => Markers,
                        buffer => State#state.buffer,
                        text => State#state.text_acc
                    }})
            end;
        none ->
            do_tokenize(T, State);
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(<<>>, #state{text_acc = <<>>} = State) ->
    lists:foldl(fun(Engine, Tokens) ->
                    Engine:handle_tokens(Tokens)
                end, lists:reverse(State#state.tokens), State#state.engines);
do_tokenize(<<>>, #state{text_acc = Text} = State) ->
    case handle_text(State#state.engines, Text, State#state.converter) of
        {ok, Token} ->
            do_tokenize(<<>>, State#state{
                tokens = [Token | State#state.tokens],
                text_acc = <<>>
            });
        ignore ->
            do_tokenize(<<>>, State);
        {error, Reason} ->
            {error, Reason}
    end.

handle_expr_start([Engine | Engines], Bin) ->
    case start_marker_match(Engine:markers(), Bin, []) of
        [] ->
            handle_expr_start(Engines, Bin);
        Markers ->
            {ok, {Engine, Markers}}
    end;
handle_expr_start([], _) ->
    none.

start_marker_match([#marker{start = Start} = Marker | Markers], Bin, Acc) ->
    BSize = erlang:byte_size(Bin),
    MSize = erlang:byte_size(Start),
    case Bin of
        <<Start:MSize/binary, 32>> ->
            start_marker_match(Markers, Bin, [{Marker, <<>>} | Acc]);
        <<Text:(BSize-MSize-1)/binary, Start:MSize/binary, 32>> ->
            start_marker_match(Markers, Bin, [{Marker, Text} | Acc]);
        _ ->
            start_marker_match(Markers, Bin, Acc)
    end;
start_marker_match([], _, Acc) ->
    Acc.

handle_expr_end(<<H, T/binary>>, Markers, Acc0) ->
    Acc = <<Acc0/binary, H>>,
    case end_marker_match(Markers, Acc) of
        {ok, {Marker, Text, Expr}} ->
            {ok, {Marker, Text, Expr, T}};
        none ->
            handle_expr_end(T, Markers, Acc)
    end;
handle_expr_end(<<>>, _, _) ->
    none.

end_marker_match([{#marker{final = Final} = Marker, Text} | Markers], Bin) ->
    BSize = erlang:byte_size(Bin),
    MSize = erlang:byte_size(Final),
    case Bin of
        <<Expr:(BSize-MSize-1)/binary, 32, Final:MSize/binary>> ->
            {ok, {Marker, Text, Expr}};
        _ ->
            end_marker_match(Markers, Bin)
    end;
end_marker_match([], _) ->
    none.

handle_text(Engines, Bin, Converter) ->
    handle_text(Engines, Bin, Bin, Converter).

handle_text([Engine | Engines], Bin, RawBin, Converter) ->
    case Engine:handle_text(Bin, Converter) of
        {ok, {text, Text}} when is_binary(Text) ->
            handle_text(Engines, Text, RawBin, Converter);
        {ok, {expr, {Expr, #marker{} = Marker, Vars}}} when is_binary(Expr), is_list(Vars) ->
            new_expr_token(RawBin, Expr, Engine, Marker, Vars);
        ignore ->
            ignore;
        {error, Reason} ->
            {error, Reason}
    end;
handle_text([], Text, RawBin, _) ->
    new_text_token(RawBin, Text).

handle_expr(Engine, Marker, Bin, Converter) ->
    case Engine:handle_expr(Marker, Bin, Converter) of
        {ok, {text, Text}} when is_binary(Text) ->
            new_text_token(Bin, Text);
        {ok, {expr, {Expr, Vars}}} when is_binary(Expr), is_list(Vars) ->
            new_expr_token(Bin, Expr, Engine, Marker, Vars);
        ignore ->
            ignore;
        {error, Reason} ->
            {error, Reason}
    end.

new_text_token(Text, HandledText) ->
    % case binary_to_ast(normalize_text_binary(HandledText)) of
        % {ok, AST} ->
            {ok, #text_token{
                text = Text,
                handled_text = HandledText
                % handled_text = normalize_text_binary(HandledText)
            }}.
        % {error, Reason} ->
            % {error, Reason}
    % end.

% normalize_text_binary(<<>>) ->
%     <<>>;
normalize_text_binary(Text) ->
    <<"<<\"", Text/binary, "\">>">>.

new_expr_token(Expr, HandledExpr, Engine, Marker, Vars) ->
    % case binary_to_ast(HandledExpr) of
        % {ok, AST} ->
            {ok, #expr_token{
                expr = Expr,
                handled_expr = HandledExpr,
                engine = Engine,
                marker = Marker,
                vars = Vars
            }}.
        % {error, Reason} ->
            % {error, Reason}
    % end.

% binary_to_ast(<<>>) ->
%     [];
binary_to_ast(Bin) ->
    case erl_scan:string(binary_to_list(iolist_to_binary([Bin, $.]))) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, AST} ->
                    {ok, AST};
                {error, Reason} ->
                    % TODO: Error
                    % error(Reason)
                    {error, Reason}
            end;
        {error, ErrorInfo, ErrorLocation} ->
            {error, {ErrorInfo, ErrorLocation}}
    end.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

% tokenize_test() ->
%     Expected = [
%         {text,<<"Hello, ">>},
%         {expr, {?SMART_ENGINE,expr,<<"World">>}},
%         {text,<<"!<p>">>},
%         {expr, {?SMART_ENGINE,expr_start,<<"case Bool of">>}},
%         {text,<<>>},
%         {expr, {?SMART_ENGINE,expr_continue,<<"true ->">>}},
%         {text,<<"True">>},
%         {expr, {?SMART_ENGINE,expr_continue,<<"; false ->">>}},
%         {text,<<>>},
%         {expr, {?SMART_ENGINE,expr_start,<<"case Foo of ->">>}},
%         {text,<<>>},
%         {expr, {?SMART_ENGINE,expr_continue,<<"foo ->">>}},
%         {text,<<"Foo">>},
%         {expr, {?SMART_ENGINE,expr_continue,<<"Bar ->">>}},
%         {text,<<"Bar">>},
%         {expr, {?SMART_ENGINE,expr_end,<<"end">>}},
%         {text,<<>>},
%         {expr, {?SMART_ENGINE,expr_end,<<"end">>}},
%         {text,<<"</p>">>}
%     ],
%     Bin = <<
%         "Hello, <%= @world .%>!"
%         "<p>"
%             "<%= case @bool of %>"
%             "<% true -> %>"
%                 "True"
%             "<% ; false -> %>"
%                 "<%= case @foo of -> %>"
%                 "<% foo -> %>"
%                     "Foo"
%                 "<% ; _ -> %>"
%                     "<p><%= @bar .%></p>"
%                 "<% end .%>"
%             "<% end .%>"
%         "</p>"
%     >>,
%     ?assertEqual(Expected, tokenize(Bin)).

tree_test() ->
    Bin = <<
        "Hello, <%= @world .%>!"
        "<p>"
            "<%= case @bool of %>"
            "<% true -> %>"
                "True"
            "<% ; false -> %>"
                "<%= case @foo of %>"
                "<% foo -> %>"
                    "Foo"
                "<% ; _ -> %>"
                    "<p><%= @bar .%></p>"
                "<% end .%>"
            "<% end .%>"
        "</p>"
        "<ul>"
            "<%= lists:map(fun(Item) -> %>"
                "<%% TODO: Items to binary .%>"
                "<li><%= @prefix .%><%= integer_to_binary(Item) .%></li>"
            "<% end, [1,2,3]) .%>"
        "</ul>"
    >>,
    Tokens = tokenize(Bin),
    {Root, Tree} = tokens_tree(Tokens),
    % ?debugFmt("~n~p", [Tree]),

    % Normalized = tree_vertex_children(Root, Tree),
    % ?debugFmt("~n~p", [Normalized]),

    % Result = v2_eel_smart_engine:handle_tree(Normalized),
    % ?debugFmt("~n~p", [Result]),

    % CTree = #{vertices => handle_compile(Tree)},
    % FIXME: Vertex 17
    {P, V, D, _} = fold_compile(Root, Tree),
    % ?debugFmt("~n~p", [CTree]),

    Bindings = #{world => <<"World">>, bool => false, foo => bar, bar => <<"baz">>, prefix => <<"Item - ">>},
%     % RenderState = handle_render(Root, CTree, Bindings),
%     % ?debugFmt("~n~p", [iolist_to_binary(RenderState)]),

%     ok.

% parts_test() ->
    State = #{
        parts => P,
        vars => V,
        dynamics => D
        % parts => #{
        %     0 => <<"Hello ">>,
        %     1 => merl:quote("maps:get(world, Bindings)"),
        %     2 => <<"!<p><p>">>,
        %     3 => merl:quote("maps:get(from, Bindings)"),
        %     4 => <<"</p></p>">>
        % },
        % vars => [
        %     {world, 1},
        %     {from, 3}
        % ]
    },

    % ?debugFmt("~p", [State]),

    % Bindings = #{world => <<"World">>, from => <<"Erlang">>},
    % Indexes = get_vars_indexes(maps:get(vars, State), Bindings),

    % NOTE: Below we render all dynamics
    Indexes = maps:get(dynamics, State),
    Parts = maps:get(parts, State),
    Changes = eval_parts(Indexes, Parts, #{'Bindings' => Bindings}),
    ?debugFmt("[Changes] ~p", [Changes]),
    Parts2 = maps:merge(Parts, Changes),
    % FIXME: Flatten list
    ?debugFmt("[Send this to client] ~p", [Parts2]),

    % NOTE: Below we render just changes
    Bindings2 = #{world => <<"William">>},
    Indexes2 = get_vars_indexes(maps:get(vars, State), Bindings2),
    Changes2 = eval_parts(Indexes2, Parts, #{'Bindings' => Bindings2}),
    ?debugFmt("[Changes2] ~p", [Changes2]),
    Parts3 = maps:merge(Parts2, Changes2),

    Bindings3 = #{prefix => <<"N - ">>},
    Indexes3 = get_vars_indexes(maps:get(vars, State), Bindings3),
    Changes3 = eval_parts(Indexes3, Parts, #{'Bindings' => Bindings3}),
    ?debugFmt("[Changes3] ~p", [Changes3]),
    Parts4 = maps:merge(Parts3, Changes3),

    ?debugFmt("[RESULT] ~p", [maps:values(Parts4)]),

    ok.

-endif.

% FIXME
get_vars_indexes(Vars0, Bindings) ->
    Vars = maps:keys(Bindings),
    sets:to_list(
        lists:foldl(fun(Var, Set0) ->
            Indexes = proplists:get_all_values(Var, Vars0),

            % ?debugFmt("[I] ~p~n", [{Var, Indexes}]),

            lists:foldl(fun(Index, Set) ->
                sets:add_element(Index, Set)
            end, Set0, Indexes)
        end, sets:new([{version, 2}]), Vars)
    ).
    % Keys = maps:keys(Bindings),
    % Vars = maps:with(Keys, Vars0),
    % sets:to_list(
    %     maps:fold(fun(_, Indexes, Set0) ->
    %         lists:foldl(fun(Index, Set) ->
    %             sets:add_element(Index, Set)
    %         end, Set0, Indexes)
    %     end, sets:new([{version, 2}]), Vars)
    % ).

eval_parts(Indexes, Parts, Bindings) ->
    lists:foldl(fun(Index, Acc) ->
        Acc#{Index => eval_part(Index, Parts, Bindings)}
    end, #{}, Indexes).

eval_part(Index, Parts, Bindings) ->
    Expr = maps:get(Index, Parts),
    case erl_eval:exprs(Expr, Bindings) of
        {value, IOData, _} when is_binary(IOData) ->
            IOData;
        {value, [IOData], _} ->
            IOData;
        {value, IOData, _} ->
            % NOTE: Here we have a list that can be optimized to the changes return.
            %       We need to know the static x dynamics of the expression.
            % ?debugFmt("[TODO: Optimize list] ~p~n", [{Index, IOData}]),
            IOData
    end.

% -------------------------------------------------

tokens_tree(Tokens) ->
    Tree0 = eel_tree:new(),
    {VRoot, Tree1} = eel_tree:add_vertex(Tree0, #{metadata => #master_vertex{}}),
    MVertices = [eel_tree:get_vertex_label(VRoot)],
    State = #tree_state{master_vertices = MVertices},
    Tree = eel_tree:set_metadata(State, Tree1),
    tokens_tree(Tokens, VRoot, Tree).

tokens_tree(Tokens, VParent, Tree) ->
    lists:foldl(
        fun
            (#text_token{} = Token, {VParent0, Tree0}) ->
                {_SVertex, Tree1} = add_slave_vertex(Token, VParent0, Tree0),
                % {V, Tree1} = eel_tree:add_vertex(Tree0, #{metadata => #slave_vertex{token = Token}}),
                % Tree2 = eel_tree:add_edge(VParent0, V, Tree1),
                {VParent0, Tree1};
            (#expr_token{marker = Marker} = Token, {VParent0, Tree0}) ->
                resolve_marker_tree_behaviors(
                    Marker#marker.tree_behavior,
                    Token,
                    VParent0,
                    Tree0
                );
            (List, {VParent0, Tree0}) when is_list(List) ->
                tokens_tree(List, VParent0, Tree0)
        end,
        {VParent, Tree},
        Tokens
    ).

resolve_marker_tree_behaviors(Behaviors, Token, VParent0, Tree0) ->
    lists:foldl(
        fun(Behavior, {VParent, Tree}) ->
            resolve_tree_behavior(Behavior, Token, VParent, Tree)
        end,
        {VParent0, Tree0},
        Behaviors
    ).

resolve_tree_behavior(push_token, Token, VParent0, Tree0) ->
    {_SVertex, Tree} = add_slave_vertex(Token, VParent0, Tree0),

    % {V, Tree1} = eel_tree:add_vertex(Tree0, #{metadata => #slave_vertex{token = Token}}),
    % Tree = eel_tree:add_edge(VParent0, V, Tree1),

    {VParent0, Tree};
resolve_tree_behavior(add_vertex, _Token, VParent0, Tree0) ->
    {MVertex, Tree} = add_master_vertex(VParent0, Tree0),

    % {VParent, Tree1} = eel_tree:add_vertex(Tree0, #{metadata => #master_vertex{}}),
    % Tree = eel_tree:add_edge(VParent0, VParent, Tree1),

    {MVertex, Tree};
% resolve_tree_behavior(push_token, Token, VParent0, Tree0) ->
%     resolve_tree_behavior({push_token, #{metadata => Token}}, Token, VParent0, Tree0);
% resolve_tree_behavior({push_token, Opts}, _Token, VParent0, Tree0) ->
%     {V, Tree1} = eel_tree:add_vertex(Tree0, Opts),
%     Tree = eel_tree:add_edge(VParent0, V, Tree1),
%     {VParent0, Tree};
% resolve_tree_behavior(add_vertex, Token, VParent0, Tree0) ->
%     resolve_tree_behavior({add_vertex, #{metadata => subtree}}, Token, VParent0, Tree0);
% resolve_tree_behavior({add_vertex, Opts}, _Token, VParent0, Tree0) ->
%     {VParent, Tree1} = eel_tree:add_vertex(Tree0, Opts),
%     Tree = eel_tree:add_edge(VParent0, VParent, Tree1),
%     {VParent, Tree};
resolve_tree_behavior(fetch_vertex_parent, _Token, VParent0, Tree) ->
    VParent = eel_tree:fetch_vertex_parent(VParent0, Tree),
    {VParent, Tree};
resolve_tree_behavior(ignore_token, _Token, VParent, Tree) ->
    {VParent, Tree}.
% TODO: Accept a custom function.
% resolve_tree_behavior(push_empty_text_token, _Token, VParent, Tree0) ->
%     {ok, Token} = new_text_token(<<>>, <<>>),
%     {V, Tree1} = eel_tree:add_vertex(Tree0, #{metadata => Token}),
%     Tree = eel_tree:add_edge(VParent, V, Tree1),
%     {VParent, Tree}.

add_slave_vertex(Token, MVertex, Tree0) ->
    Opts = #{metadata => #slave_vertex{token = Token}},
    {SVertex, Tree1} = eel_tree:add_vertex(Tree0, Opts),
    Tree = eel_tree:add_edge(MVertex, SVertex, Tree1),
    {SVertex, Tree}.

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

% tree_vertex_children(Vertex0, Tree) ->
%     Vertex = eel_tree:fetch_vertex(Vertex0, Tree),
%     Children = eel_tree:fetch_vertex_children(Vertex, Tree),
%     lists:foldl(fun (Child, Acc) ->
%                     [tree_vertex_metadata(Child, Tree) | Acc]
%                 end, [], Children).
% tree_vertex_metadata(Vertex, Tree) ->
%     Metadata = eel_tree:get_vertex_metadata(Vertex),
%     normalize_tree_vertex_metadata(Metadata, Vertex, Tree).

% normalize_tree_vertex_metadata(#text_token{} = Token, _Vertex, _) ->
%     Token;
%     % Token#text_token.handled_text;
% normalize_tree_vertex_metadata(#expr_token{} = Token, _Vertex, _) ->
%     Token;
%     % Token#expr_token.handled_expr;
% normalize_tree_vertex_metadata(subtree, Vertex, Tree) ->
%     tree_vertex_children(Vertex, Tree).

handle_compile(Tree) ->
    % Subtrees: 0,4,6,9,10,12,15
    % IMPORTANT! We can always compile them.
    % ?debugFmt("~p~n", [Tree]),
    % Root = eel_tree:get_root(Tree),


    State = eel_tree:get_metadata(Tree),
    MVertices = State#tree_state.master_vertices,
    lists:foldl(
        fun(Label, Acc) ->
            Acc#{
                % Label => do_compile_master_vertex(Label, Tree)
                Label => fold_compile(Label, Tree)
            }

            % Vertex = eel_tree:fetch_vertex(Label, Tree),
            % Expr = do_compile_vertex_children(Vertex, Tree),
            % String =
            %     binary_to_list(
            %         iolist_to_binary([Expr, $.])
            %     ),
            % {ok, Tokens, _} = erl_scan:string(String),
            % {ok, AST} = erl_parse:parse_exprs(Tokens),
            % Acc#{
            %     Label => #{
            %         expr => Expr,
            %         ast => AST
            %     }
            % }

            % M = eel_tree:get_vertex_metadata(Vertex),
            % Metadata = M#master_vertex{
            %     ast = E
            % },
            % V = eel_tree:set_vertex_metadata(Metadata, Vertex),
            % eel_tree:put_vertex(V, Acc)
        end,
        #{},
        MVertices
    ).

    % Root = eel_tree:fetch_vertex(12, Tree),
    % S =
    %     binary_to_list(
    %         iolist_to_binary([do_compile_vertex_children(Root, Tree), $.])
    %     ),

    % {ok, T, _} = erl_scan:string(S),
    % {ok, E} = erl_parse:parse_exprs(T),
    % {value, IOList, _} = erl_eval:exprs(E, #{'Bindings' => #{world => <<"World">>, bool => false, foo => bar, bar => <<"baz">>}}),
    % iolist_to_binary(IOList).

% do_compile_master_vertex(VertexLabel, Tree) ->
%     Vertex = eel_tree:fetch_vertex(VertexLabel, Tree),
%     Children = lists:reverse(eel_tree:fetch_vertex_children(Vertex, Tree)),
%     {Ret, _} =
%         lists:foldl(
%             fun(Child, {#{statics := S, dynamics := D} = Acc, I}) ->
%                 case eel_tree:get_vertex_metadata(Child) of
%                     #master_vertex{} ->
%                         {Acc#{
%                             dynamics => [#{
%                                 index => I,
%                                 vertex => eel_tree:get_vertex_label(Child)
%                             } | D]
%                         }, I+1};
%                     #slave_vertex{token = #text_token{} = Token} ->
%                         Static = <<"<<\"", (Token#text_token.handled_text)/binary, "\"/utf8>>">>,
%                         {Acc#{statics => [Static | S]}, I+1};
%                     #slave_vertex{token = #expr_token{marker = #marker{id = expr}} = Token} ->
%                         {Acc#{
%                             dynamics => [#{
%                                 index => I,
%                                 vertex => eel_tree:get_vertex_label(Child),
%                                 expr => Token#expr_token.handled_expr,
%                                 vars => Token#expr_token.vars
%                             } | D]
%                         }, I+1};
%                     #slave_vertex{token = #expr_token{} = Token} ->
%                         {Acc#{
%                             dynamics => [#{
%                                 hey => here,
%                                 index => I,
%                                 vertex => eel_tree:get_vertex_label(Child),
%                                 expr => Token#expr_token.handled_expr,
%                                 vars => Token#expr_token.vars
%                             } | D]
%                         }, I+1}
%                 end
%             end,
%             {#{statics => [], dynamics => []}, 0},
%             Children
%         ),
%     Ret.
    % Ret#{
    %     statics => lists:reverse(maps:get(statics, Ret)),
    %     dynamics => lists:reverse(maps:get(dynamics, Ret))
    % }.

fold_compile(VertexLabel, Tree) ->
    Vertex = eel_tree:fetch_vertex(VertexLabel, Tree),
    State = {#{}, [], [], 0},
    do_fold_compile(Vertex, false, State, Tree).

do_fold_compile(Vertex, Rec, State, Tree) ->
    Children = lists:reverse(eel_tree:fetch_vertex_children(Vertex, Tree)),
    do_fold_compile_0(Children, Rec, State, Tree).

do_fold_compile_0([Vertex | T] = Vertices, Rec, State, Tree) ->
    Metadata = eel_tree:get_vertex_metadata(Vertex),
    case do_fold_compile_1(Metadata, Rec, State, Vertices, Tree) of
        {halt, State1} ->
            State1;
        State1 ->
            do_fold_compile_0(T, Rec, State1, Tree)
        end;
do_fold_compile_0([], Rec, {Acc, V, D, I} = State, _) ->
    case maps:find(I, Acc) of
        {ok, Expr} ->
            {Acc#{I => lists:join($,, lists:reverse(Expr))}, V, D, I};
            % case Rec of
            %     true ->
            %         {Acc#{I => lists:join($,, lists:reverse(Expr))}, V, D, I};
            %     false ->
            %         ?debugFmt("[E] ~p~n", [Acc]),
            %         State
            % end;
        error ->
            State
    end.

do_fold_compile_1(#master_vertex{}, Rec, State, [Vertex | _], Tree) ->
    do_fold_compile(Vertex, Rec, State, Tree);
do_fold_compile_1(#slave_vertex{token = Token}, Rec, State, Vertices, Tree) ->
    do_fold_compile_2(Token, Rec, State, Vertices, Tree).

do_fold_compile_2(#text_token{} = Token, false, {Acc, V, D, I}, _Vertices, _Tree) ->
    Static = Token#text_token.handled_text,
    {Acc#{I => Static}, V, D, I+1};
do_fold_compile_2(#text_token{} = Token, true, {Acc, V, D, I}, _Vertices, _Tree) ->
    Static = Token#text_token.handled_text,
    % {Acc#{I => <<"<<\"", Static/binary, "\"/utf8>>">>}, V, I};
    {Acc#{I => [<<"<<\"", Static/binary, "\"/utf8>>">> | maps:get(I, Acc, [])]}, V, D, I};
do_fold_compile_2(#expr_token{marker = #marker{id = expr}} = Token, false, {Acc, V, D, I}, [Vertex | _], _Tree) ->
    {ok, AST} = binary_to_ast(Token#expr_token.handled_expr),
    VMap = lists:map(fun(TV) -> {TV, I} end, Token#expr_token.vars),
    {Acc#{I => AST}, lists:merge(V, VMap), [I | D], I+1};
do_fold_compile_2(#expr_token{marker = #marker{id = expr}} = Token, true, {Acc, V, D, I}, [Vertex | _], _Tree) ->
    % {ok, AST} = binary_to_ast(Token#expr_token.handled_expr),
    VMap = lists:map(fun(TV) -> {TV, I} end, Token#expr_token.vars),
    {Acc#{I => [Token#expr_token.handled_expr | maps:get(I, Acc, [])]}, lists:merge(V, VMap), D, I};
do_fold_compile_2(#expr_token{marker = #marker{id = expr_start}} = Token, false, State, [Vertex | Vertices], Tree) ->
    {{Acc, V, D, I}, {Expr, Vars}} = do_fold_compile_3(Vertices, {Token#expr_token.handled_expr, Token#expr_token.vars}, false, State, Tree),
    {ok, AST} = binary_to_ast(Expr),
    VMap = lists:map(fun(TV) -> {TV, I} end, Token#expr_token.vars),
    {halt, {Acc#{I => AST}, lists:merge(V, VMap), [I | D], I+1}};
% FIXME: Issue here
do_fold_compile_2(#expr_token{marker = #marker{id = expr_start}} = Token, true, {AA, _, _, _} = State, [Vertex | Vertices], Tree) ->
    % ?debugFmt("~p~n", [AA]),

    {{Acc, V, D, I}, {Expr, Vars}} = do_fold_compile_3(Vertices, {Token#expr_token.handled_expr, Token#expr_token.vars}, true, State, Tree),
    VMap = lists:map(fun(TV) -> {TV, I} end, Token#expr_token.vars),
    % {ok, AST} = binary_to_ast(Expr),
    {halt, {Acc#{I => Expr}, lists:merge(V, VMap), D, I+1}}.

do_fold_compile_3([Vertex | T], Acc, Rec, State, Tree) ->
    Token = eel_tree:get_vertex_metadata(Vertex),
    {State1, Acc1} = do_fold_compile_4(Token, Acc, Vertex, Rec, State, Tree),
    do_fold_compile_3(T, Acc1, Rec, State1, Tree);
do_fold_compile_3([], Acc, _, State, _) ->
    {State, Acc}.

do_fold_compile_4(#master_vertex{}, {E, V}, Vertex, Rec, {A, Vars, D, I} = State, Tree) ->
    % Children = lists:reverse(eel_tree:fetch_vertex_children(Vertex, Tree)),
    {Comp, VV, D1, II} = do_fold_compile(Vertex, true, {#{}, Vars, D, I}, Tree),
    % ?debugFmt("[COM] ~p~n", [erl_syntax:revert(erl_syntax:abstract(maps:values(Comp)))]),

    % ?debugFmt("~p~n", [hd(maps:values(Comp))]),

    Expr=maps:values(Comp),
        % case Rec of
        %     true ->
        %         lists:join($,, hd(maps:values(Comp)));
        %     false ->
        %         maps:values(Comp)
        % end,
    % ?debugFmt("[COM] ~p~n", [Expr]),

    % State1 = do_fold_compile(Vertex, false, {A, I}, Tree),


    VertexLabel = integer_to_binary(I),
        % case eel_tree:get_vertex_label(Vertex) of
        %     VL when is_integer(VL) ->
        %         integer_to_binary(VL);
        %     VL when is_atom(VL) ->
        %         atom_to_binary(VL);
        %     VL when is_binary(VL) ->
        %         VL;
        %     VL when is_list(VL) ->
        %         list_to_binary(VL)
        % end,
    % E1 = <<E/binary, 32, "v2_eel_renderer:render_part(", VertexLabel/binary, ", Tree, Bindings)">>,
    E1 = [E, 32, $[, Expr, $], 32],
    {{A, VV, D1, I}, {E1, V}};
do_fold_compile_4(#slave_vertex{token = #expr_token{handled_expr = Expr}}, {E, V}, _, Rec, State, _) ->
    E1 = [E, 32, Expr],
    {State, {E1, V}}.

% TODO: Engine handler
% do_compile_vertex_children(Vertex0, Tree) ->
%     Vertex = eel_tree:fetch_vertex(Vertex0, Tree),
%     case eel_tree:get_vertex_metadata(Vertex) of
%         #master_vertex{} ->
%             Children = eel_tree:fetch_vertex_children(Vertex, Tree),
%             First = lists:last(Children),
%             FirstMeta = eel_tree:get_vertex_metadata(First),
%             Expr =
%                 lists:reverse(lists:map(
%                     fun(Child) ->
%                         do_compile_vertex_children(Child, Tree)
%                     end,
%                     Children
%                 )),
%             case FirstMeta of
%                 #slave_vertex{token =
%                     #expr_token{marker = #marker{id = expr_start}}
%                 } ->
%                     Expr;
%                 _ ->
%                     [ $[, lists:join(<<$,>>, Expr), $] ]
%             end;
%         #slave_vertex{token = #text_token{} = Token} ->
%             <<"<<\"", (Token#text_token.handled_text)/binary, "\"/utf8>>">>;
%         #slave_vertex{token = #expr_token{} = Token} ->
%             Token#expr_token.handled_expr
%     end.


% handle_render(Vertex, Tree) ->
%     handle_render(Vertex, Tree, #{}).

% handle_render(Vertex0, Tree, Bindings) ->
%     Vertex = eel_tree:fetch_vertex(Vertex0, Tree),
%     Metadata = eel_tree:get_vertex_metadata(Vertex),
%     AST = Metadata#master_vertex.ast,
%     {value, IOList, _} = erl_eval:exprs(AST, #{'Bindings' => Bindings}),
%     IOList.
