-module(v2_eel_smart_engine).

-export([ markers/0
        , handle_text/2
        , handle_expr/3
        , handle_tokens/1
        , handle_tree/1
        ]).

-include("v2_eel.hrl").

markers() ->
    [
        % #marker{
        %     id = snapshot,
        %     start = <<"<%.">>,
        %     final = <<".%>">>,
        %     tree_behavior = push
        % },
        #marker{
            id = expr,
            start = <<"<%=">>,
            final = <<".%>">>,
            tree_behavior = [push_token]
        },
        #marker{
            id = expr_start,
            start = <<"<%=">>,
            final = <<"%>">>,
            % tree_behavior = [add_vertex, push_empty_text_token, push_token]
            tree_behavior = [add_vertex, push_token, add_vertex]
        },
        #marker{
            id = expr_continue,
            start = <<"<%">>,
            final = <<"%>">>,
            tree_behavior = [fetch_vertex_parent, push_token, add_vertex]
        },
        #marker{
            id = expr_end,
            start = <<"<%">>,
            final = <<".%>">>,
            tree_behavior = [fetch_vertex_parent, push_token, fetch_vertex_parent]
        },
        #marker{
            id = comment,
            start = <<"<%%">>,
            final = <<"%%>">>,
            tree_behavior = [ignore_token]
        }
    ].

% handle_text(<<>>, _) ->
%     ignore;
handle_text(Text, _) ->
    {ok, {text, Text}}.

% handle_expr(_Marker, Expr, _Converter) ->
%     {ok, {expr, {Expr, []}}}.
handle_expr(Marker, Expr0, Converter) ->
    Expr1 = replace_expr_vars(Expr0, <<>>),
    Expr = normalize_expr(Marker, Expr1, Converter),
    Vars = collect_expr_vars(Expr0, []),
    {ok, {expr, {Expr, Vars}}}.

% NOTE: We should always start an iolist when we have a non empty binary
%       after a expr_start, e.g
%       [
%           {expr_start, <<"case @foo of foo -> ">>},
%           {text, <<"<p>">>},
%           {expr, <<"@bar">>},
%           {text, <<"</p>">>}
%           {expr_end, <<"end">>}
%       ] =
%       case maps:get(foo, Bindings) of
%           foo ->
%               [<<"<p>">>, maps:get(bar, Bindings), <<"</p>">>]
%       end.

% FIXME: Ignore when inside quotes (single [atom] and double [string]).
replace_expr_vars(<<$@, T0/binary>>, Acc) ->
    {T, Var} = collect_expr_var(T0, <<>>),
    replace_expr_vars(T, <<Acc/binary, "maps:get(", Var/binary, ", Bindings)">>);
replace_expr_vars(<<H, T/binary>>, Acc) ->
    replace_expr_vars(T, <<Acc/binary, H>>);
replace_expr_vars(<<>>, Acc) ->
    Acc.

normalize_expr(_, Expr, _) ->
    Expr.
% normalize_expr(#marker{id = expr}, Expr, Converter) ->
%     <<Converter/binary, ":to_string(begin ", Expr/binary, " end).">>;
% normalize_expr(#marker{id = expr_start}, Expr, Converter) ->
%     <<Converter/binary, ":to_string(begin ", Expr/binary>>;
% normalize_expr(#marker{id = expr_continue}, Expr, _) ->
%     Expr;
% normalize_expr(#marker{id = expr_end}, Expr, _) ->
%     <<Expr/binary, " end).">>;
% normalize_expr(#marker{id = comment}, _, _) ->
%     <<>>.

collect_expr_vars(<<$@, T0/binary>>, Acc) ->
    {T, Var} = collect_expr_var(T0, <<>>),
    collect_expr_vars(T, [binary_to_atom(Var) | Acc]);
collect_expr_vars(<<_, T/binary>>, Acc) ->
    collect_expr_vars(T, Acc);
collect_expr_vars(<<>>, Acc) ->
    Acc.

collect_expr_var(<<32, _/binary>> = T, Acc) ->
    {T, Acc};
collect_expr_var(<<H, T/binary>>, Acc) ->
    collect_expr_var(T, <<Acc/binary, H>>);
collect_expr_var(<<>>, <<>>) ->
    error(badarg);
collect_expr_var(<<>>, Acc) ->
    {<<>>, Acc}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

handle_tokens(Tokens) ->
    {Reversed, _} = lists:foldl(
        fun resolve_tokens_acc/2,
        % fun
        %     % Expr
        %     (#expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = Token,
        %     {[#expr_token{marker = #marker{id = expr_start}, engine = ?MODULE} = TAcc | Acc], {in_expr, false}}) ->
        %         {[join_expr_tokens(Token, TAcc) | Acc], {in_continue, false}};
        %     (#expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = Token,
        %     {[#expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = TAcc | Acc], {in_continue, false}}) ->
        %         {[join_expr_tokens(Token, TAcc) | Acc], {in_continue, false}};
        %     (#expr_token{marker = #marker{id = expr_end}, engine = ?MODULE} = Token,
        %     {[#expr_token{marker = #marker{id = expr_start}, engine = ?MODULE} = TAcc | Acc], {in_expr, false}}) ->
        %         {[join_expr_tokens(Token, TAcc) | Acc], {in_expr, false}};
        %     (#expr_token{marker = #marker{id = expr_end}, engine = ?MODULE} = Token,
        %     {[#expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = TAcc | Acc], {in_continue, false}}) ->
        %         {[join_expr_tokens(Token, TAcc) | Acc], {in_text, false}};
        %     (#expr_token{marker = #marker{id = expr_start}, engine = ?MODULE} = Token, {Acc, _}) ->
        %         {[Token | Acc], {in_expr, false}};

        %     % Do not use
        %     % (#expr_token{marker = #marker{id = expr_start}, engine = ?MODULE} = Token,
        %     % {[#text_token{text = <<>>} = PrevToken | Acc], _}) ->
        %     %     {[Token, start_expr_arr(PrevToken) | Acc], {in_expr, true}};
        %     % (#expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = Token, {[PrevToken | Acc], _}) ->
        %     %     {[Token, close_expr_arr(PrevToken) | Acc], {in_continue, false}};
        %     % (#expr_token{marker = #marker{id = expr_end}, engine = ?MODULE} = Token, {[PrevToken | Acc], _}) ->
        %     %     {[Token, close_expr_arr(PrevToken) | Acc], {in_text, false}};
        %     % (Token,
        %     % {[#expr_token{marker = #marker{id = MarkerId}, engine = ?MODULE} | _] = Acc, {In, false}})
        %     % when MarkerId =:= expr_start; MarkerId =:= expr_continue ->
        %     %     {[start_expr_arr(Token) | Acc], {In, true}};
        %     % (Token, {Acc, {In, true}}) ->
        %     %     {[continue_expr_arr(Token) | Acc], {In, true}};

        %     % List, also issued
        %     % (#expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = Token, {[PrevToken, Acc], _}) ->
        %     %     {[[Token | PrevToken], Acc], in_continue};
        %     % (#expr_token{marker = #marker{id = expr_end}, engine = ?MODULE} = Token, {[PrevToken, Acc], _}) ->
        %     %     {[lists:reverse([Token | PrevToken]), Acc], in_text};
        %     % (Token,
        %     % {[#expr_token{marker = #marker{id = MarkerId}, engine = ?MODULE} | _] = Acc, State})
        %     % when MarkerId =:= expr_start; MarkerId =:= expr_continue ->
        %     %     {[[Token], Acc], State};

        %     (#expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = Token,
        %     {[#text_token{text = <<>>} | Acc], _}) ->
        %         {[Token | Acc], {in_expr, true}};

        %     % Text
        %     (#text_token{} = Token,
        %     {[#text_token{} = TAcc | Acc], State}) ->
        %         {[join_text_tokens(Token, TAcc) | Acc], State};
        %     (#text_token{} = Token, {Acc, State}) ->
        %         {[Token | Acc], State};
        %     % Other
        %     (Token, {Acc, State}) ->
        %         {[Token | Acc], State}
        % end,
        {[], {in_text, false}},
        Tokens
    ),
    lists:reverse(Reversed).

 % Expr
 resolve_tokens_acc(
    #expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = Token,
    {[#expr_token{marker = #marker{id = expr_start}, engine = ?MODULE} = TAcc | Acc], {in_expr, false}}
) ->
     {[join_expr_tokens(Token, TAcc) | Acc], {in_continue, false}};
resolve_tokens_acc(
    #expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = Token,
    {[#expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = TAcc | Acc], {in_continue, false}}
) ->
     {[join_expr_tokens(Token, TAcc) | Acc], {in_continue, false}};
resolve_tokens_acc(
    #expr_token{marker = #marker{id = expr_end}, engine = ?MODULE} = Token,
    {[#expr_token{marker = #marker{id = expr_start}, engine = ?MODULE} = TAcc | Acc], {in_expr, false}}
) ->
     {[join_expr_tokens(Token, TAcc) | Acc], {in_expr, false}};
resolve_tokens_acc(
    #expr_token{marker = #marker{id = expr_end}, engine = ?MODULE} = Token,
    {[#expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = TAcc | Acc], {in_continue, false}}
) ->
     {[join_expr_tokens(Token, TAcc) | Acc], {in_text, false}};
resolve_tokens_acc(
    #expr_token{marker = #marker{id = expr_start}, engine = ?MODULE} = Token,
    {Acc, _}
) ->
     {[Token | Acc], {in_expr, false}};

 % Do not use
 % (#expr_token{marker = #marker{id = expr_start}, engine = ?MODULE} = Token,
 % {[#text_token{text = <<>>} = PrevToken | Acc], _}) ->
 %     {[Token, start_expr_arr(PrevToken) | Acc], {in_expr, true}};
 % (#expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = Token, {[PrevToken | Acc], _}) ->
 %     {[Token, close_expr_arr(PrevToken) | Acc], {in_continue, false}};
 % (#expr_token{marker = #marker{id = expr_end}, engine = ?MODULE} = Token, {[PrevToken | Acc], _}) ->
 %     {[Token, close_expr_arr(PrevToken) | Acc], {in_text, false}};
 % (Token,
 % {[#expr_token{marker = #marker{id = MarkerId}, engine = ?MODULE} | _] = Acc, {In, false}})
 % when MarkerId =:= expr_start; MarkerId =:= expr_continue ->
 %     {[start_expr_arr(Token) | Acc], {In, true}};
 % (Token, {Acc, {In, true}}) ->
 %     {[continue_expr_arr(Token) | Acc], {In, true}};

 % List, also issued
 % (#expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = Token, {[PrevToken, Acc], _}) ->
 %     {[[Token | PrevToken], Acc], in_continue};
 % (#expr_token{marker = #marker{id = expr_end}, engine = ?MODULE} = Token, {[PrevToken, Acc], _}) ->
 %     {[lists:reverse([Token | PrevToken]), Acc], in_text};
 % (Token,
 % {[#expr_token{marker = #marker{id = MarkerId}, engine = ?MODULE} | _] = Acc, State})
 % when MarkerId =:= expr_start; MarkerId =:= expr_continue ->
 %     {[[Token], Acc], State};

resolve_tokens_acc(
    #text_token{text = <<>>},
    {[#expr_token{engine = ?MODULE} | _] = Acc, In}
) ->
    {Acc, In};
resolve_tokens_acc(
    #expr_token{engine = ?MODULE} = Token,
    {[#text_token{text = <<>>} | Acc], In}
) ->
    resolve_tokens_acc(Token, {Acc, In});

 % Text
resolve_tokens_acc(
    #text_token{} = Token,
    {[#text_token{} = TAcc | Acc], State}
) ->
     {[join_text_tokens(Token, TAcc) | Acc], State};
resolve_tokens_acc(
    #text_token{} = Token,
    {Acc, State}
) ->
     {[Token | Acc], State};
 % Other
resolve_tokens_acc(
    Token,
    {Acc, State}
) ->
     {[Token | Acc], State}.


join_text_tokens(Discard, Keep) ->
    Keep#text_token{
        text = <<(Keep#text_token.text)/binary, 32,
                 (Discard#text_token.text)/binary>>,
        handled_text = <<(Keep#text_token.handled_text)/binary, 32,
                         (Discard#text_token.handled_text)/binary>>
    }.

join_expr_tokens(Discard, Keep) ->
    Keep#expr_token{
        expr = <<(Keep#expr_token.expr)/binary, 32,
                 (Discard#expr_token.expr)/binary>>,
        handled_expr = <<(Keep#expr_token.handled_expr)/binary, 32,
                         (Discard#expr_token.handled_expr)/binary>>,
        vars = lists:merge(Discard#expr_token.vars, Keep#expr_token.vars)
    }.

start_expr_arr(#expr_token{} = Token) ->
    Token#expr_token{
        expr = <<$[, (Token#expr_token.expr)/binary>>,
        handled_expr = <<$[, (Token#expr_token.handled_expr)/binary>>
    };
start_expr_arr(#text_token{} = Token) ->
    Token#text_token{
        text = <<$[, (Token#text_token.text)/binary>>,
        handled_text = <<$[, (Token#text_token.handled_text)/binary>>
    }.

continue_expr_arr(#expr_token{} = Token) ->
    Token#expr_token{
        expr = <<$,, (Token#expr_token.expr)/binary>>,
        handled_expr = <<$,, (Token#expr_token.handled_expr)/binary>>
    };
continue_expr_arr(#text_token{} = Token) ->
    Token#text_token{
        text = <<$,, (Token#text_token.text)/binary>>,
        handled_text = <<$,, (Token#text_token.handled_text)/binary>>
    }.

close_expr_arr(#expr_token{} = Token) ->
    Token#expr_token{
        expr = <<(Token#expr_token.expr)/binary, $]>>,
        handled_expr = <<(Token#expr_token.handled_expr)/binary, $]>>
    };
close_expr_arr(#text_token{} = Token) ->
    Token#text_token{
        text = <<(Token#text_token.text)/binary, $]>>,
        handled_text = <<(Token#text_token.handled_text)/binary, $]>>
    }.

handle_tree(Tree) ->
    Tree.
    % ?debugFmt("~p", [Tree]),
%     do_handle_tree(Tree, []).

% do_handle_tree(Tree, Acc0) when is_list(Tree) ->
%     lists:reverse(
%         lists:foldl(
%             fun
%                 (#text_token{handled_text = HandledText}, Acc) ->
%                     [HandledText | Acc];
%                 (#expr_token{handled_expr = HandledExpr}, Acc) ->
%                     [HandledExpr | Acc];
%                 (List, Acc) ->
%                     [do_handle_tree(List, []) | Acc]
%             end,
%             Acc0,
%             Tree
%         )
%     ).
