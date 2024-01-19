-module(eel_smart_engine).

-export([ markers/0
        , handle_text/1
        , handle_expr/2
        , handle_tokens/1
        , handle_tree/1
        , normalize_expr/1
        , get_expr_vars/1
        ]).

-include("eel.hrl").

markers() ->
    [
        #marker{
            id = expr,
            start = <<"<%=\\s+">>,
            final = <<"\\s+.%>">>,
            tree_behaviors = [push_token],
            compile_as = expr
        },
        #marker{
            id = expr_start,
            start = <<"<%=\\s+">>,
            final = <<"\\s+%>">>,
            tree_behaviors = [add_vertex, push_token, add_vertex],
            compile_as = expr_start
        },
        #marker{
            id = expr_continue,
            start = <<"<%\\s+">>,
            final = <<"\\s+%>">>,
            tree_behaviors = [fetch_vertex_parent, push_token, add_vertex]
        },
        #marker{
            id = expr_end,
            start = <<"<%\\s+">>,
            final = <<"\\s+.%>">>,
            tree_behaviors = [fetch_vertex_parent, push_token, fetch_vertex_parent]
        },
        #marker{
            id = comment,
            start = <<"<%%\\s+">>,
            final = <<"\\s+%%>">>,
            tree_behaviors = [ignore_token]
        }
    ].

handle_text(_Text) ->
    next.

handle_expr(Marker, Expr0) ->
    Expr = normalize_expr(Expr0),
    Vars = get_expr_vars(Expr0),
    {ok, [{expr, {Marker, Expr, Vars}}]}.

normalize_expr(Expr) ->
    replace_expr_vars(Expr, <<>>).

% FIXME: Ignore when inside quotes (single [atom] and double [string]).
replace_expr_vars(<<$@, T0/binary>>, Acc) ->
    {Var, T1} = find_var_ending(T0),
    case find_var_default(T1) of
        {Default0, T} ->
            Default = normalize_expr(Default0),
            replace_expr_vars(T, <<Acc/binary,
                "(maps:get(", Var/binary, ", Bindings, ", Default/binary, "))"
            >>);
        none ->
            replace_expr_vars(T1, <<Acc/binary,
                "(maps:get(", Var/binary, ", Bindings))"
            >>)
    end;
replace_expr_vars(<<H, T/binary>>, Acc) ->
    replace_expr_vars(T, <<Acc/binary, H>>);
replace_expr_vars(<<>>, Acc) ->
    Acc.

get_expr_vars(Expr) ->
    get_expr_vars(Expr, []).

get_expr_vars(<<$@, T0/binary>>, Acc) ->
    {Var, T} = find_var_ending(T0),
    get_expr_vars(T, [binary_to_atom(Var) | Acc]);
get_expr_vars(<<_, T/binary>>, Acc) ->
    get_expr_vars(T, Acc);
get_expr_vars(<<>>, Acc) ->
    Acc.

find_var_ending(<<H, T/binary>>) when H >= $a andalso H =< $z ->
    find_var_ending(T, <<H>>);
find_var_ending(_) ->
    error(badarg).

find_var_ending(<<H, T/binary>>, Acc)
  when (H >= $a andalso H =< $z)
     ; (H >= $A andalso H =< $A)
     ; (H >= $0 andalso H =< $9)
     ; H =:= $_
     ; H =:= $@ ->
    find_var_ending(T, <<Acc/binary, H>>);
find_var_ending(T, Var) ->
    {Var, T}.

% FIXME: Ignore when inside quotes (single [atom] and double [string]).
% TODO: Improve the default syntax or remove it.
%       Currently, this is valid:
%       1> @foo \\ @bar \\ baz. = (maps:get(foo, Bindings, (maps:get(bar, Bindings, baz))))
%       The required dot at the end is strange, specially where the comma
%       is required:
%       2> #{foo => @foo \\ foo., bar => @bar \\ @baz.}
%       The '\\' comes from Elixir's default syntax on functions.
find_var_default(<<$\s, T/binary>>) ->
    find_var_default(T);
% Erlang ignores the first backslash, so only one is considered.
find_var_default(<<$\\, T/binary>>) ->
    find_var_default_ending(T, <<>>);
find_var_default(_) ->
    none.

find_var_default_ending(<<$., T/binary>>, Default) ->
    {Default, T};
find_var_default_ending(<<$\s, T/binary>>, <<>>) ->
    find_var_default_ending(T, <<>>);
find_var_default_ending(<<H, T/binary>>, Acc) ->
    find_var_default_ending(T, <<Acc/binary, H>>);
find_var_default_ending(<<>>, Default) ->
    {Default, <<>>}.

handle_tokens(Tokens) ->
    Acc = {[], {in_text, false}},
    {Reversed, _} = lists:foldl(fun resolve_tokens_acc/2, Acc, Tokens),
    lists:reverse(Reversed).

 % Expr
 resolve_tokens_acc(
    #expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = Token,
    { [#expr_token{marker = #marker{id = expr_start}, engine = ?MODULE} = TAcc | Acc]
    , {in_expr, false} }
) ->
     {[merge_expr_tokens(Token, TAcc) | Acc], {in_continue, false}};
resolve_tokens_acc(
    #expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = Token,
    { [#expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = TAcc | Acc]
    , {in_continue, false} }
) ->
     {[merge_expr_tokens(Token, TAcc) | Acc], {in_continue, false}};
resolve_tokens_acc(
    #expr_token{marker = #marker{id = expr_end}, engine = ?MODULE} = Token,
    { [#expr_token{marker = #marker{id = expr_start}, engine = ?MODULE} = TAcc | Acc]
    , {in_expr, false} }
) ->
     {[merge_expr_tokens(Token, TAcc) | Acc], {in_expr, false}};
resolve_tokens_acc(
    #expr_token{marker = #marker{id = expr_end}, engine = ?MODULE} = Token,
    { [#expr_token{marker = #marker{id = expr_continue}, engine = ?MODULE} = TAcc | Acc]
    , {in_continue, false} }
) ->
     {[merge_expr_tokens(Token, TAcc) | Acc], {in_text, false}};
resolve_tokens_acc(
    #expr_token{marker = #marker{id = expr_start}, engine = ?MODULE} = Token,
    {Acc, _In}
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
     {[merge_text_tokens(Token, TAcc) | Acc], State};
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

merge_text_tokens(Discard, Keep) ->
    Keep#text_token{
        text = merge_bin(Keep#text_token.text, Discard#text_token.text)
    }.

merge_expr_tokens(Discard, Keep) ->
    Keep#expr_token{
        expr = merge_bin(Keep#expr_token.expr, Discard#expr_token.expr),
        vars = lists:merge(Discard#expr_token.vars, Keep#expr_token.vars)
    }.

merge_bin(A, B) ->
    <<A/binary, 32, B/binary>>.

handle_tree(Tree) ->
    Tree.
