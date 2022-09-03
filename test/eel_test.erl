-module(eel_test).

-include_lib("eunit/include/eunit.hrl").

-define(RE_WS_TRIM, re:compile(<<"^\\s+|\\s+$">>)).

% -type dynamic() :: {
%     ExprRef    :: atom(),
%     Markers    :: {StartMarker :: binary(), EndMarker :: binary()},
%     Expr       :: binary(),
%     UniqueVars :: [atom()],
%     Parts      :: {Static :: [binary(), Dynamic :: [atom()]},
% }.

tokenize_test() ->
    Bin = <<
        "<html>"
        "<div>"
        "<%=   Foo = 1, Bar = Foo   .%>"
        "</div>"
        "<%=   case #{foo := Foo} = Map of %>"
        "<%   bar -> Bar; %>"
        "<%   foobar -> Foobar; %>"
        "<%   Foo -> Foo end .%>"
        "</html>"
    >>,
    Expected = {
        {
            [
                <<"<html><div>">>,
                <<>>,
                <<"</div>">>,
                <<>>,
                <<"</html>">>
            ],
            [
                [
                    {expr, {<<"=">>, <<".">>}, <<"Foo = 1, Bar = Foo.">>, ['Foo', 'Bar'],
                        {[<<>>, <<" = 1, ">>, <<" = ">>, <<".">>], ['Foo', 'Bar', 'Foo']}}
                ],
                [
                    {start_expr, {<<"=">>, <<" ">>}, <<"case #{foo := Foo} = Map of">>, ['Map'],
                        {[<<"case #{foo := Foo} = ">>, <<" of">>], ['Map']}},
                    {mid_expr, {<<" ">>, <<" ">>}, <<"bar -> Bar;">>, ['Bar'],
                        {[<<"bar -> ">>, <<";">>], ['Bar']}},
                    {mid_expr, {<<" ">>, <<" ">>}, <<"foobar -> Foobar;">>, ['Foobar'],
                        {[<<"foobar -> ">>, <<";">>], ['Foobar']}},
                    {end_expr, {<<" ">>, <<".">>}, <<"Foo -> Foo end.">>, ['Foo'],
                        {[<<>>, <<" -> ">>, <<" end.">>], ['Foo', 'Foo']}}
                ]
            ]
        },
        <<"<html><div><%= Foo = 1, Bar = Foo .%></div><%= case #{foo := Foo} = Map of %><% bar -> Bar; %><% foobar -> Foobar; %><% Foo -> Foo end .%></html>">>
    },
    ?assertEqual(Expected, tokenize(Bin)).

tokenize(Bin) ->
    do_tokenize(Bin, {[], []}, [], <<>>).

do_tokenize(<<"<%", _/binary>> = Bin, {Static0, Dynamic0}, TokensAcc, Acc0) ->
    case tokenize_expr(Bin, Acc0) of
        {ok, {ExprRef, Token, Rest, Acc}} ->
            {Static, Dynamic1} =
                case lists:member(ExprRef, [expr, start_expr]) of
                    true ->
                        {[<<>> | Static0], [[Token] | Dynamic0]};
                    false ->
                        [DH0 | DT0] = Dynamic0,
                        {Static0, [[Token | DH0] | DT0]}
                end,
            Dynamic =
                case ExprRef =:= end_expr of
                    true ->
                        [DH1 | DT1] = Dynamic1,
                        [lists:reverse(DH1) | DT1];
                    false ->
                        Dynamic1
                end,
            do_tokenize(Rest, {Static, Dynamic}, [Token | TokensAcc], Acc);
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(<<H, T/binary>>, {Static0, Dynamic}, TokensAcc0, Acc) ->
    {TokenStatic, Static} =
        case Static0 of
            [] ->
                {<<H>>, [<<H>>]};
            [SH | ST] ->
                case TokensAcc0 of
                    [{text, _} | _] -> {<<SH/binary, H>>, [<<SH/binary, H>> | ST]};
                    _ -> {<<H>>, [<<H>>, SH | ST]}
                end
        end,
    Token = {text, TokenStatic},
    TokensAcc =
        case TokensAcc0 of
            [{text, _} | TT] -> [Token | TT];
            _ -> [Token | TokensAcc0]
        end,
    do_tokenize(T, {Static, Dynamic}, TokensAcc, <<Acc/binary, H>>);
do_tokenize(<<>>, {Static, Dynamic}, _TokensAcc, Acc) ->
    {{lists:reverse(Static), lists:reverse(Dynamic)}, Acc}.

tokenize_expr(<<"<%", T0/binary>>, Acc0) ->
    {StartMarker, MaybeSpace, T} = retrieve_marker(T0, <<>>),
    Acc1 = <<Acc0/binary, "<%", StartMarker/binary, MaybeSpace/binary>>,
    case tokenize_expr(T, StartMarker, <<>>, Acc1) of
        {ok, {ExprRef, Expr0, EndMarker, Rest, Acc}} ->
            Expr =
                case lists:member(ExprRef, [expr, end_expr]) andalso EndMarker =:= <<".">> of
                    true -> <<Expr0/binary, $.>>;
                    false -> Expr0
                end,
            Vars = retrieve_vars(Expr),
            Parts = split_expr(Expr, Vars),
            UniqueVars = unique(Vars),

            Token = {ExprRef, {StartMarker, EndMarker}, Expr, UniqueVars, Parts},
            {ok, {ExprRef, Token, Rest, Acc}};
        {error, Reason} ->
            {error, Reason}
    end.

tokenize_expr(<<32, "%>", T/binary>>, StartMarker, Cache, Acc) ->
    Expr = trim(Cache),
    ExprRef =
        case StartMarker of
            <<32>> -> mid_expr;
            _ -> start_expr
        end,
    {ok, {ExprRef, Expr, <<32>>, T, <<Acc/binary, Expr/binary, " %>">>}};
tokenize_expr(<<32, EndMarker, "%>", T/binary>>, StartMarker, Cache, Acc) ->
    Expr = trim(Cache),
    ExprRef =
        case StartMarker of
            <<32>> -> end_expr;
            _ -> expr
        end,
    {ok, {ExprRef, Expr, <<EndMarker>>, T, <<Acc/binary, Expr/binary, 32, EndMarker, "%>">>}};
tokenize_expr(<<"<%", _/binary>>, _StartMarker, _Cache, _Acc) ->
    % TODO: Handle unknown marker
    {error, unknown_end_marker};
tokenize_expr(<<H, T/binary>>, StartMarker, Cache, Acc) ->
    tokenize_expr(T, StartMarker, <<Cache/binary, H>>, Acc);
tokenize_expr(<<>>, _StartMarker, _Cache, _Acc) ->
    {error, eof}.

retrieve_marker(<<32, T/binary>>, <<>>) ->
    {<<32>>, <<>>, T};
retrieve_marker(<<32, _/binary>> = T, Marker) ->
    {Marker, <<32>>, T};
retrieve_marker(<<H, T/binary>>, Marker) ->
    retrieve_marker(T, <<Marker/binary, H>>);
retrieve_marker(<<>>, _Marker) ->
    {error, eof}.

trim(Bin) ->
    {ok, RE} = ?RE_WS_TRIM,
    re:replace(Bin, RE, "", [{return, binary}, global]).

retrieve_vars(Expr0) ->
    Expr = erlang:binary_to_list(Expr0),
    {ok, Tokens, _} = erl_scan:string(Expr),
    do_retrieve_vars(Tokens, []).

do_retrieve_vars([{':=', _}, {var, _, _} | Tokens], Acc) ->
    do_retrieve_vars(Tokens, Acc);
do_retrieve_vars([{var, _, Var} | Tokens], Acc0) ->
    Acc = [Var | Acc0],
    do_retrieve_vars(Tokens, Acc);
do_retrieve_vars([_Token | Tokens], Acc) ->
    do_retrieve_vars(Tokens, Acc);
do_retrieve_vars([], Acc) ->
    lists:reverse(Acc).

split_expr(Expr, Vars) ->
    VarsBin = lists:map(fun erlang:atom_to_binary/1, Vars),
    Static = binary:split(Expr, VarsBin, [global]),
    {Static, Vars}.

unique([]) -> [];
unique([H | T]) -> [H | [X || X <- unique(T), X =/= H]].
