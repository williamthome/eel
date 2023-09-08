-module(v2_eel_smart_engine).

-export([ markers/0
        , handle_text/2
        , handle_expr/3
        , handle_tokens/1
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
            tree_behavior = push
        },
        #marker{
            id = expr_start,
            start = <<"<%=">>,
            final = <<"%>">>,
            tree_behavior = open
        },
        #marker{
            id = expr_continue,
            start = <<"<%">>,
            final = <<"%>">>,
            tree_behavior = push
        },
        #marker{
            id = expr_end,
            start = <<"<%">>,
            final = <<".%>">>,
            tree_behavior = close
        },
        #marker{
            id = comment,
            start = <<"<%%">>,
            final = <<"%%>">>,
            tree_behavior = ignore
        }
    ].

handle_text(Text, _) ->
    {ok, {text, Text}}.

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

replace_expr_vars(<<$@, T0/binary>>, Acc) ->
    {T, Var} = collect_expr_var(T0, <<>>),
    replace_expr_vars(T, <<Acc/binary, "maps:get(", Var/binary, ", Bindings)">>);
replace_expr_vars(<<H, T/binary>>, Acc) ->
    replace_expr_vars(T, <<Acc/binary, H>>);
replace_expr_vars(<<>>, Acc) ->
    Acc.

normalize_expr(#marker{id = expr}, Expr, Converter) ->
    <<Converter/binary, ":to_binary(", Expr/binary, ").">>;
normalize_expr(#marker{id = expr_start}, Expr, Converter) ->
    <<Converter/binary, ":to_binary(", Expr/binary>>;
normalize_expr(#marker{id = expr_continue}, Expr, _) ->
    Expr;
normalize_expr(#marker{id = expr_end}, Expr, _) ->
    <<Expr/binary, ").">>;
normalize_expr(#marker{id = comment}, _, _) ->
    <<>>.

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

handle_tokens(Tokens) ->
    Tokens.
