-module(eel_smart_engine).
-behaviour(eel_engine).

%% eel_engine callbacks
-export([ init/1, handle_text/2, handle_expr/3, handle_tokens/1 ]).

%% API
-export([ normalize_expr/1, get_expr_vars/1 ]).

-include("eel.hrl").

init(_Opts) ->
    {ok, #engine_state{
        markers = [
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
                % FIXME: Nested comments causes error.
                tree_behaviors = [ignore_token]
            }
        ]
    }}.

handle_text(_Text, State) ->
    {ok, State}.

handle_expr(Marker, Expr0, State0) ->
    Expr = normalize_expr(Expr0),
    Vars = get_expr_vars(Expr0),
    ExprToken = #expr_token{
        engine = ?MODULE,
        marker = Marker,
        expr = Expr,
        vars = Vars
    },
    State = eel_tokenizer:push_token(ExprToken, State0),
    {ok, State}.

normalize_expr(Expr) ->
    replace_expr_vars(Expr, <<>>).

% FIXME: Ignore when inside quotes (single [atom] and double [string]).
% TODO: Improve code readability.
% TODO: Support records and nested records.
% TODO: Union of ok and nested returns. Both should return vars to support
%       multiple defaults assigns.
replace_expr_vars(<<$@, T0/binary>>, Acc) ->
    case find_var_ending(T0) of
        {ok, {Var, T1}} ->
            case find_var_default(T1) of
                {Default0, T} ->
                    Default = normalize_expr(Default0),
                    replace_expr_vars(T, <<Acc/binary,
                        "(maps:get(", Var/binary, ", Assigns, ", Default/binary, "))"
                    >>);
                none ->
                    replace_expr_vars(T1, <<Acc/binary,
                        "(maps:get(", Var/binary, ", Assigns))"
                    >>)
            end;
        % Provides nested maps getting, e.g.:
        %   <%= @foo.baz.baz \\ 5 .%> = (maps:get(baz, (maps:get(bar, (maps:get(foo, Assigns)))), 5))
        {nested, {[Var | Rest], T1}} ->
            First = <<"(maps:get(", Var/binary, ", Assigns))">>,
            case Rest of
                [] ->
                    replace_expr_vars(T1, <<Acc/binary,
                        First/binary
                    >>);
                [Last] ->
                    case find_var_default(T1) of
                        {Default0, T2} ->
                            Default = normalize_expr(Default0),
                            replace_expr_vars(T2, <<Acc/binary,
                                "(maps:get(", Last/binary, ", ", First/binary, ", ", Default/binary, "))"
                            >>);
                        none ->
                            replace_expr_vars(T1, <<Acc/binary,
                                "(maps:get(", Last/binary, ", ", First/binary, "))"
                            >>)
                    end;
                Rest ->
                    Others = lists:foldl(fun(V, VAcc) ->
                        <<"(maps:get(", V/binary, ", ", VAcc/binary, "))">>
                    end, First, lists:droplast(Rest)),
                    Last = lists:last(Rest),
                    case find_var_default(T1) of
                        {Default0, T2} ->
                            Default = normalize_expr(Default0),
                            replace_expr_vars(T2, <<Acc/binary,
                                "(maps:get(", Last/binary, ", ", Others/binary, ", ", Default/binary, "))"
                            >>);
                        none ->
                            replace_expr_vars(T1, <<Acc/binary,
                                "(maps:get(", Last/binary, ", ", Others/binary,"))"
                            >>)
                    end
            end
    end;
replace_expr_vars(<<H, T/binary>>, Acc) ->
    replace_expr_vars(T, <<Acc/binary, H>>);
replace_expr_vars(<<>>, Acc) ->
    Acc.

get_expr_vars(Expr) ->
    get_expr_vars(Expr, []).

get_expr_vars(<<$@, T0/binary>>, Acc) ->
    {Var, T} =
        case find_var_ending(T0) of
            {ok, {V, Rest}} ->
                {V, Rest};
            {nested, {[V | _], Rest}} ->
                {V, find_expr_vars_ending(Rest)}
        end,
    get_expr_vars(T, [binary_to_atom(Var) | Acc]);
get_expr_vars(<<_, T/binary>>, Acc) ->
    get_expr_vars(T, Acc);
get_expr_vars(<<>>, Acc) ->
    lists:reverse(Acc).

find_expr_vars_ending(T0) ->
    case find_var_ending(T0, <<>>) of
        {ok, {_Var, T}} ->
            T;
        {nested, {_Var, T}} ->
            find_expr_vars_ending(T)
    end.

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
find_var_ending(<<$., Next, T/binary>>, Var) when Next >= $a, Next =< $z ->
    {nested, find_nested_var_ending(<<Next, T/binary>>, [Var])};
find_var_ending(T, Var) ->
    {ok, {Var, T}}.

find_nested_var_ending(<<>>, Acc) ->
    {Acc, <<>>};
find_nested_var_ending(T0, Acc) ->
    case find_var_ending(T0, <<>>) of
        {ok, {<<>>, T}} ->
            {Acc, T};
        {ok, {Var, T}} ->
            {Acc ++ [Var], T};
        {nested, {Vars, T}} ->
            find_nested_var_ending(T, Acc ++ Vars)
    end.

% FIXME: Ignore when inside quotes (single [atom] and double [string]).
% TODO: Improve the default syntax or remove it.
%       Currently, this is valid:
%       1> @foo \\ @bar \\ baz. = (maps:get(foo, Assigns, (maps:get(bar, Assigns, baz))))
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

% FIXME: Should ignore symbols when a inside string.
% TODO: Check more symbols.
find_var_default_ending(<<$\r, _/binary>> = T, Default) ->
    {Default, T};
find_var_default_ending(<<$\n, _/binary>> = T, Default) ->
    {Default, T};
find_var_default_ending(<<$,, _/binary>> = T, Default) ->
    {Default, T};
find_var_default_ending(<<$., _/binary>> = T, Default) ->
    {Default, T};
find_var_default_ending(<<$}, _/binary>> = T, Default) ->
    {Default, T};
find_var_default_ending(<<$], _/binary>> = T, Default) ->
    {Default, T};
% TODO: Should check this space implementation after drop the required dot.
find_var_default_ending(<<$\s, T/binary>>, <<>>) ->
    find_var_default_ending(T, <<>>);
find_var_default_ending(<<$\s, _/binary>> = T, Default) ->
    {Default, T};
find_var_default_ending(<<H, T/binary>>, Acc) ->
    find_var_default_ending(T, <<Acc/binary, H>>);
find_var_default_ending(<<>>, Default) ->
    {Default, <<>>}.

handle_tokens(State0) ->
    Tokens0 = eel_tokenizer:get_tokens(State0),
    Acc = {[], {in_text, false}},
    {Reversed, _} = lists:foldl(fun resolve_tokens_acc/2, Acc, Tokens0),
    Tokens = lists:reverse(Reversed),
    State = eel_tokenizer:set_tokens(Tokens, State0),
    {ok, State}.

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
% NOTE: Merge texts cause Arizona components to break.
% resolve_tokens_acc(
%     #text_token{} = Token,
%     {[#text_token{} = TAcc | Acc], State}
% ) ->
%      {[merge_text_tokens(Token, TAcc) | Acc], State};
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

% merge_text_tokens(Discard, Keep) ->
%     Keep#text_token{
%         text = merge_bin(Keep#text_token.text, Discard#text_token.text)
%     }.

merge_expr_tokens(Discard, Keep) ->
    Keep#expr_token{
        expr = merge_bin(Keep#expr_token.expr, Discard#expr_token.expr),
        vars = lists:merge(Discard#expr_token.vars, Keep#expr_token.vars)
    }.

merge_bin(A, B) ->
    <<A/binary, $\s, B/binary>>.
