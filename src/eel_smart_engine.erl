-module(eel_smart_engine).

-export([ markers/0
        , handle_text/1
        , handle_expr/2
        , handle_tokens/1
        , handle_tree/1
        ]).

-include("eel.hrl").

markers() ->
    [
        #marker{
            id = expr,
            start = <<"<%=">>,
            final = <<".%>">>,
            tree_behaviors = [push_token]
        },
        #marker{
            id = expr_start,
            start = <<"<%=">>,
            final = <<"%>">>,
            tree_behaviors = [add_vertex, push_token, add_vertex]
        },
        #marker{
            id = expr_continue,
            start = <<"<%">>,
            final = <<"%>">>,
            tree_behaviors = [fetch_vertex_parent, push_token, add_vertex]
        },
        #marker{
            id = expr_end,
            start = <<"<%">>,
            final = <<".%>">>,
            tree_behaviors = [fetch_vertex_parent, push_token, fetch_vertex_parent]
        },
        #marker{
            id = comment,
            start = <<"<%%">>,
            final = <<".%>">>,
            tree_behaviors = [ignore_token]
        }
    ].

handle_text(Text) ->
    {ok, [{text, Text}]}.

handle_expr(Marker, Expr0) ->
    Expr = replace_expr_vars(Expr0, <<>>),
    Vars = collect_expr_vars(Expr0, []),
    {ok, [{expr, {Marker, Expr, Vars}}]}.

% FIXME: Ignore when inside quotes (single [atom] and double [string]).
replace_expr_vars(<<$@, T0/binary>>, Acc) ->
    {T, Var} = collect_expr_var(T0, <<>>),
    replace_expr_vars(T, <<Acc/binary, "maps:get(", Var/binary, ", Bindings)">>);
replace_expr_vars(<<H, T/binary>>, Acc) ->
    replace_expr_vars(T, <<Acc/binary, H>>);
replace_expr_vars(<<>>, Acc) ->
    Acc.

collect_expr_vars(<<$@, T0/binary>>, Acc) ->
    {T, Var} = collect_expr_var(T0),
    collect_expr_vars(T, [binary_to_atom(Var) | Acc]);
collect_expr_vars(<<_, T/binary>>, Acc) ->
    collect_expr_vars(T, Acc);
collect_expr_vars(<<>>, Acc) ->
    Acc.

collect_expr_var(<<H, T/binary>>) when H >= $a andalso H =< $z ->
    collect_expr_var(T, <<H>>);
collect_expr_var(_) ->
    error(badarg).

collect_expr_var(<<H, T/binary>>, Acc) when (H >= $a andalso H =< $z)
                                          ; (H >= $A andalso H =< $A)
                                          ; (H >= $0 andalso H =< $9)
                                          ; H =:= $_
                                          ; H =:= $@ ->
    collect_expr_var(T, <<Acc/binary, H>>);
collect_expr_var(T, Acc) ->
    {T, Acc}.

handle_tokens(Tokens) ->
    Acc = {[], {in_text, false}},
    {Reversed, _} = lists:foldl(fun resolve_tokens_acc/2, Acc, Tokens),
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
resolve_tokens_acc(
    #expr_token{engine = ?MODULE} = Token,
    {[#text_token{text = <<>>} | Acc], In}
) ->
    resolve_tokens_acc(Token, {Acc, In});
% Text
resolve_tokens_acc(
    #text_token{text = <<>>},
    {[#expr_token{engine = ?MODULE} | _] = Acc, In}
) ->
    {Acc, In};
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
% None
resolve_tokens_acc(
    Token,
    {Acc, State}
) ->
     {[Token | Acc], State}.

join_text_tokens(Discard, Keep) ->
    Keep#text_token{
        text = <<(Keep#text_token.text)/binary, 32,
                 (Discard#text_token.text)/binary>>
    }.

join_expr_tokens(Discard, Keep) ->
    Keep#expr_token{
        expr = <<(Keep#expr_token.expr)/binary, 32,
                 (Discard#expr_token.expr)/binary>>,
        vars = lists:merge(Discard#expr_token.vars, Keep#expr_token.vars)
    }.

handle_tree(Tree) ->
    Tree.
