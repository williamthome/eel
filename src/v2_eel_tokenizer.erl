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
        {ok, {expr, {Expr, Vars}}} when is_binary(Expr), is_list(Vars) ->
            new_expr_token(RawBin, Expr, Engine, none, Vars);
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
    case erl_scan:string(binary_to_list(<<Bin/binary, $.>>)) of
        {ok, Tokens, _} ->
            case erl_parse:parse_form(Tokens) of
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
    % Tokens = [{text_token,<<"Hello, ">>,<<"Hello, ">>},
    % {expr_token,<<"@world">>,
    %             <<"v2_eel_converter:to_binary(maps:get(world, Bindings)).">>,
    %             v2_eel_smart_engine,
    %             {marker,expr,<<"<%=">>,<<".%>">>,push},
    %             [world]},
    % {text_token,<<"!<p>">>,<<"!<p>">>},
    % {expr_token,<<"case @bool of">>,
    %             <<"v2_eel_converter:to_binary(case maps:get(bool, Bindings) of">>,
    %             v2_eel_smart_engine,
    %             {marker,expr_start,<<"<%=">>,<<"%>">>,open},
    %             [bool]},
    % {text_token,<<>>,<<>>},
    % {expr_token,<<"true ->">>,<<"true ->">>,v2_eel_smart_engine,
    %             {marker,expr_continue,<<"<%">>,<<"%>">>,push},
    %             []},
    % {text_token,<<"True">>,<<"True">>},
    % {expr_token,<<"; false ->">>,<<"; false ->">>,
    %             v2_eel_smart_engine,
    %             {marker,expr_continue,<<"<%">>,<<"%>">>,push},
    %             []},
    % {text_token,<<>>,<<>>},
    % {expr_token,<<"case @foo of ->">>,
    %             <<"v2_eel_converter:to_binary(case maps:get(foo, Bindings) of ->">>,
    %             v2_eel_smart_engine,
    %             {marker,expr_start,<<"<%=">>,<<"%>">>,open},
    %             [foo]},
    % {text_token,<<>>,<<>>},
    % {expr_token,<<"foo ->">>,<<"foo ->">>,v2_eel_smart_engine,
    %             {marker,expr_continue,<<"<%">>,<<"%>">>,push},
    %             []},
    % {text_token,<<"Foo">>,<<"Foo">>},
    % {expr_token,<<"; _ ->">>,<<"; _ ->">>,v2_eel_smart_engine,
    %             {marker,expr_continue,<<"<%">>,<<"%>">>,push},
    %             []},
    % {text_token,<<"<p>">>,<<"<p>">>},
    % {expr_token,<<"@bar">>,
    %             <<"v2_eel_converter:to_binary(maps:get(bar, Bindings)).">>,
    %             v2_eel_smart_engine,
    %             {marker,expr,<<"<%=">>,<<".%>">>,push},
    %             [bar]},
    % {text_token,<<"</p>">>,<<"</p>">>},
    % {expr_token,<<"end">>,<<"end).">>,v2_eel_smart_engine,
    %             {marker,expr_end,<<"<%">>,<<".%>">>,close},
    %             []},
    % {text_token,<<>>,<<>>},
    % {expr_token,<<"end">>,<<"end).">>,v2_eel_smart_engine,
    %             {marker,expr_end,<<"<%">>,<<".%>">>,close},
    %             []},
    % {text_token,<<"</p>">>,<<"</p>">>}],

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
    >>,
    Tokens = tokenize(Bin),
    % ?debugFmt("~p", [Tokens]),

    {Root, Tree} = tokens_tree(Tokens),
    % ?debugFmt("~n~p", [Tree]),

    % Normalized = tree_vertex_children(Root, Tree),
    % ?debugFmt("~n~p", [Normalized]),

    % Result = v2_eel_smart_engine:handle_tree(Normalized),
    % ?debugFmt("~n~p", [Result]),

    ?debugFmt("~n~p", [handle_compile(Tree)]),

    ok.

-endif.

tokens_tree(Tokens) ->
    {VRoot, Tree} = eel_tree:add_vertex(eel_tree:new(), #{metadata => subtree}),
    tokens_tree(Tokens, VRoot, Tree).

tokens_tree(Tokens, VParent, Tree) ->
    lists:foldl(
        fun
            (#text_token{} = Token, {VParent0, Tree0}) ->
                {V, Tree1} = eel_tree:add_vertex(Tree0, #{metadata => Token}),
                Tree2 = eel_tree:add_edge(VParent0, V, Tree1),
                {VParent0, Tree2};
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
    {V, Tree1} = eel_tree:add_vertex(Tree0, #{metadata => Token}),
    Tree = eel_tree:add_edge(VParent0, V, Tree1),
    {VParent0, Tree};
resolve_tree_behavior(add_vertex, _Token, VParent0, Tree0) ->
    {VParent, Tree1} = eel_tree:add_vertex(Tree0, #{metadata => subtree}),
    Tree = eel_tree:add_edge(VParent0, VParent, Tree1),
    {VParent, Tree};
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

tree_vertex_children(Vertex0, Tree) ->
    Vertex = eel_tree:fetch_vertex(Vertex0, Tree),
    Children = eel_tree:fetch_vertex_children(Vertex, Tree),
    lists:foldl(fun (Child, Acc) ->
                    [tree_vertex_metadata(Child, Tree) | Acc]
                end, [], Children).
tree_vertex_metadata(Vertex, Tree) ->
    Metadata = eel_tree:get_vertex_metadata(Vertex),
    normalize_tree_vertex_metadata(Metadata, Vertex, Tree).

normalize_tree_vertex_metadata(#text_token{} = Token, _Vertex, _) ->
    Token;
    % Token#text_token.handled_text;
normalize_tree_vertex_metadata(#expr_token{} = Token, _Vertex, _) ->
    Token;
    % Token#expr_token.handled_expr;
normalize_tree_vertex_metadata(subtree, Vertex, Tree) ->
    tree_vertex_children(Vertex, Tree).

handle_compile(Tree) ->
    Root = eel_tree:get_root(Tree),
    compile_vertex_children(Root, Tree, {[], []}).
    % eel_tree:map_vertices(
    %     fun(_Id, Vertex) ->
    %         compile_vertex_children(Vertex, Tree)
    %     end,
    %     Tree
    % ).

compile_vertex_children(Vertex0, Tree, {TAcc, EAcc} = Acc) ->
    Vertex = eel_tree:fetch_vertex(Vertex0, Tree),
    case eel_tree:get_vertex_metadata(Vertex) of
        subtree ->
            Children = eel_tree:fetch_vertex_children(Vertex, Tree),

            ?debugFmt("~p~n", [Children]),

            {T, Expr} =
                    lists:foldl(
                        fun
                            (Child, Acc0) ->
                                compile_vertex_children(Child, Tree, Acc0)
                        end,
                        Acc,
                        Children
                    );
            % {lists:reverse(T), lists:reverse(Expr)};
            % lists:flatten(io_lib:format("~s", [Expr]));
            % lists:join(<<",">>, Expr);
        #text_token{} = Token ->
            {[Token | TAcc], [<<"<<\"", (Token#text_token.handled_text)/binary, "\">>">> | EAcc]};
        % #expr_token{marker = #marker{id = expr_start}} = Token ->
        %     <<(Token#expr_token.handled_expr)/binary, " [">>;
        % #expr_token{marker = #marker{id = expr_continue}} = Token ->
        %     <<"] ", (Token#expr_token.handled_expr)/binary, " [">>;
        % #expr_token{marker = #marker{id = expr_end}} = Token ->
        %     <<"] ", (Token#expr_token.handled_expr)/binary>>;
        #expr_token{} = Token ->
            % ?debugFmt("~p~n", [{TAcc, Token#expr_token.handled_expr}]),
            {[Token | TAcc], [Token#expr_token.handled_expr | EAcc]}
    end.

% map_child() ->


    % lists:foldl(fun (Child, Tree0) ->
    %     case eel_tree:get_vertex_metadata(Child) of
    %         subtree ->
    %             compile_vertex_children(Child, Tree0);
    %         #expr_token{} = Token0 ->
    %             AST = binary_to_ast(Token0#expr_token.handled_expr),
    %             Token = Token0#expr_token{ast = AST},
    %             NVertex = eel_tree:set_vertex_metadata(Token, Vertex),
    %             eel_tree:put_vertex(NVertex, Tree0);
    %         _ ->
    %             Tree0
    %     end
    %     % Compiled = compile_vertex(Child, Tree0),
    %     % eel_tree:put_vertex(Compiled, Tree0)
    % end, Tree, Children).

% compile_vertex(Vertex, Tree) ->
%     Metadata = eel_tree:get_vertex_metadata(Vertex),
%     compile_tree_vertex_metadata(Metadata, Vertex, Tree).

% % compile_tree_vertex_metadata(#text_token{} = Token, Vertex) ->
% %     eel_tree:set_vertex_metadata(Token, Vertex);
% compile_tree_vertex_metadata(#text_token{}, Vertex, _) ->
%     Vertex;
% compile_tree_vertex_metadata(#expr_token{} = Token0, Vertex, _) ->
%     AST = binary_to_ast(Token0#expr_token.handled_expr),
%     Token = Token0#expr_token{ast = AST},
%     eel_tree:set_vertex_metadata(Token, Vertex);
% compile_tree_vertex_metadata(subtree, Vertex, Tree) ->
%     compile_vertex_children(Vertex, Tree).

a() ->
    _1 = [{vertex,0,21,[],false,{text_token,<<"</p>">>,<<"</p>">>,undefined}},

    {vertex,0,4,[20,9,8,6,5],false,subtree},  % <-

    {vertex,0,3,[],false,{text_token,<<"!<p>">>,<<"!<p>">>,undefined}},
    {vertex,0,2,[],false,
            {expr_token,<<"@world">>,<<"maps:get(world, Bindings)">>,
                        v2_eel_smart_engine,
                        {marker,expr,<<"<%=">>,<<".%>">>,[push_token]},
                        [world],
                        undefined}},
    {vertex,0,1,[],false,{text_token,<<"Hello, ">>,<<"Hello, ">>,undefined}}],

    _2 =  [{vertex,4,20,[],false,
    {expr_token,<<"end">>,<<"end">>,v2_eel_smart_engine,
                {marker,expr_end,<<"<%">>,<<".%>">>,
                        [fetch_vertex_parent,push_token,
                         fetch_vertex_parent]},
                [],undefined}},
{vertex,4,9,"\n",false,subtree},
{vertex,4,8,[],false,
    {expr_token,<<"; false ->">>,<<"; false ->">>,v2_eel_smart_engine,
                {marker,expr_continue,<<"<%">>,<<"%>">>,
                        [fetch_vertex_parent,push_token,add_vertex]},
                [],undefined}},
{vertex,4,6,[7],false,subtree},
{vertex,4,5,[],false,
    {expr_token,<<"case @bool of true ->">>,
                <<"case maps:get(bool, Bindings) of true ->">>,
                v2_eel_smart_engine,
                {marker,expr_start,<<"<%=">>,<<"%>">>,
                        [add_vertex,push_token,add_vertex]},
                [bool],
                undefined}}],

    % TODO: Remove commas
    % S = "[<<\"Hello, \">>,maps:get(world, Bindings),<<\"!<p>\">>,[case maps:get(bool, Bindings) of true ->,[<<\"True\">>],; false ->,[<<\"False\">>],end]].",
    % TODO: Remover colchetes
    % S = "[<<\"Hello, \">>,maps:get(world, Bindings),<<\"!<p>\">>,[case maps:get(bool, Bindings) of true -> [<<\"True\">>] ; false -> [<<\"False\">>] end]].",
    S = "[<<\"Hello, \">>,maps:get(world, Bindings),<<\"!<p>\">>,case maps:get(bool, Bindings) of true -> [<<\"True\">>] ; false -> [<<\"False\">>] end].",
    {ok, T, _} = erl_scan:string(S),
    {ok, E} = erl_parse:parse_exprs(T),
    {value, Bin, _} = erl_eval:exprs(E, #{'Bindings' => #{world => <<"World">>, bool => true}}),
    Bin.
