% TODO: Doc/Spec
-module(eel).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([scan/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

scan(Bin) ->
    scan(Bin, []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

scan(<<>>, Tokens) ->
    lists:reverse(Tokens);
scan(<<"<%", T/bitstring>>, Tokens) ->
    {Token, Rest} = guess_expr_token(T),
    scan(Rest, [Token | Tokens]);
scan(Bin, Tokens) ->
    {Expr, Rest} = guess_token(Bin),
    scan(Rest, [{text, Expr} | Tokens]).

guess_token(Bin) ->
    guess_token(Bin, <<>>).

guess_token(<<"%>", Rest/bitstring>>, Expr) ->
    {Expr, Rest};
guess_token(<<"<%", _/bitstring>> = Rest, Expr) ->
    {Expr, Rest};
guess_token(<<H, Rest/bitstring>>, Expr) ->
    guess_token(Rest, <<Expr/bitstring, H>>);
guess_token(<<>>, Expr) ->
    {Expr, <<>>}.

guess_expr_token(Bin) ->
    {Expr, Rest} = guess_token(Bin),
    First = binary:first(Expr),
    Last = binary:last(Expr),
    Space = 32,
    Token =
        case Last =:= $= of
            true ->
                case First =:= Space of
                    true ->
                        {end_expr, bin_drop_last(Expr)};
                    false ->
                        Marker = int_to_char(First),
                        {expr, Marker, bin_drop_first_and_last(Expr)}
                end;
            false ->
                case First =:= Space of
                    true ->
                        {mid_expr, Expr};
                    false ->
                        Marker = int_to_char(First),
                        {start_expr, Marker, bin_drop_first(Expr)}
                end
        end,
    {Token, Rest}.

bin_reverse(Bin) ->
    binary:encode_unsigned(binary:decode_unsigned(Bin, little)).

bin_drop_first(<<_H, T/bitstring>>) ->
    T.

bin_drop_last(Bin) ->
    Bin1 = bin_reverse(Bin),
    Bin2 = bin_drop_first(Bin1),
    bin_reverse(Bin2).

bin_drop_first_and_last(<<_H, T/bitstring>>) ->
    bin_drop_last(T).

int_to_char(Int) ->
    list_to_binary(io_lib:format("~c", [Int])).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

scan_test() ->
    ?assertEqual(
        [
            {text, <<"<ul>">>},
            {start_expr, <<"=">>, <<" lists:map(fun(Foo) -> ">>},
            {start_expr, <<"=">>, <<" case Foo of ">>},
            {mid_expr, <<" true -> ">>},
            {text, <<"<li>">>},
            {mid_expr, <<" foo; ">>},
            {text, <<"</li>">>},
            {mid_expr, <<" Bar -> ">>},
            {expr, <<"#">>, <<" Maybe a comment ">>},
            {text, <<"<li>">>},
            {mid_expr, <<" Bar ">>},
            {expr, <<"=">>, <<" Baz. ">>},
            {text, <<"</li>">>},
            {end_expr, <<" end. ">>},
            {end_expr, <<" end, List). ">>},
            {text, <<"</ul>">>}
        ],
        scan(
            <<
                "<ul>"
                "<%= lists:map(fun(Foo) -> %>"
                "<%= case Foo of %>"
                "<% true -> %>"
                "<li><% foo; %></li>"
                "<% Bar -> %>"
                "<%# Maybe a comment =%>"
                "<li><% Bar %><%= Baz. =%></li>"
                % maybe "<% end %>?"
                "<% end. =%>"
                "<% end, List). =%>"
                "</ul>"
            >>
        )
    ).

-endif.
