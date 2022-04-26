%%%-----------------------------------------------------------------------------
%%% @doc Embedded Erlang.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type level() :: non_neg_integer().
-type symbol() :: text | expr | start_expr | mid_expr | end_expr.
-type marker() :: binary().
-type syntax() :: binary().
-type token() ::
    {level(), text, syntax()}
    | {level(), expr, marker(), syntax()}
    | {level(), start_expr, marker(), syntax()}
    | {level(), mid_expr, syntax()}
    | {level(), end_expr, syntax()}.
-type tokens() :: [token()].

-export_type([
    symbol/0,
    marker/0,
    syntax/0,
    token/0
]).

-export([scan/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Scans characters to tokens.
%% @end
%%------------------------------------------------------------------------------
-spec scan(binary()) -> tokens().

scan(Bin) ->
    scan(0, Bin, []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec scan(level(), binary(), tokens()) -> tokens().

scan(_Level, <<>>, Tokens) ->
    lists:reverse(Tokens);
scan(Level, <<"<%", T/binary>>, Tokens) ->
    {NewLevel, Token, Rest} = guess_token(Level, T),
    scan(NewLevel, Rest, [Token | Tokens]);
scan(Level, Bin, Tokens) ->
    {Syntax, Rest} = guess_syntax(Bin),
    Token = {Level, text, Syntax},
    scan(Level, Rest, [Token | Tokens]).

-spec guess_syntax(binary()) -> {syntax(), binary()}.

guess_syntax(Bin) ->
    guess_syntax(Bin, <<>>).

-spec guess_syntax(binary(), binary()) -> {syntax(), binary()}.

guess_syntax(<<"%>", Rest/binary>>, Syntax) ->
    {Syntax, Rest};
guess_syntax(<<"<%", _/binary>> = Rest, Syntax) ->
    {Syntax, Rest};
guess_syntax(<<H, Rest/binary>>, Syntax) ->
    guess_syntax(Rest, <<Syntax/binary, H>>);
guess_syntax(<<>>, Syntax) ->
    {Syntax, <<>>}.

-spec guess_token(level(), binary()) -> {level(), token(), binary()}.

guess_token(Level, Bin) ->
    {Syntax, Rest} = guess_syntax(Bin),
    FirstByte = binary:first(Syntax),
    LastByte = binary:last(Syntax),
    Space = 32,
    {ResultLevel, ResultToken} =
        case LastByte =:= $= of
            true ->
                case FirstByte =:= Space of
                    true ->
                        NewLevel = Level - 1,
                        NewSyntax = bin_drop_last(Syntax),
                        Token = {Level, end_expr, NewSyntax},
                        {NewLevel, Token};
                    false ->
                        Marker = byte_to_binary(FirstByte),
                        NewSyntax = bin_drop_first_and_last(Syntax),
                        Token = {Level + 1, expr, Marker, NewSyntax},
                        {Level, Token}
                end;
            false ->
                case FirstByte =:= Space of
                    true ->
                        Token = {Level, mid_expr, Syntax},
                        {Level, Token};
                    false ->
                        NewLevel = Level + 1,
                        Marker = byte_to_binary(FirstByte),
                        NewSyntax = bin_drop_first(Syntax),
                        Token = {Level + 1, start_expr, Marker, NewSyntax},
                        {NewLevel, Token}
                end
        end,
    {ResultLevel, ResultToken, Rest}.

-spec bin_reverse(binary()) -> binary().

bin_reverse(Bin) ->
    binary:encode_unsigned(binary:decode_unsigned(Bin, little)).

-spec bin_drop_first(binary()) -> binary().

bin_drop_first(<<_H, T/binary>>) ->
    T.

-spec bin_drop_last(binary()) -> binary().

bin_drop_last(Bin) ->
    Bin1 = bin_reverse(Bin),
    Bin2 = bin_drop_first(Bin1),
    bin_reverse(Bin2).

-spec bin_drop_first_and_last(binary()) -> binary().

bin_drop_first_and_last(<<_H, T/binary>>) ->
    bin_drop_last(T).

-spec byte_to_binary(byte()) -> binary().

byte_to_binary(Byte) ->
    list_to_binary(io_lib:format("~c", [Byte])).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

scan_test() ->
    ?assertEqual(
        [
            {0, text, <<"<ul>">>},
            {1, start_expr, <<"=">>, <<" lists:map(fun(Foo) -> ">>},
            {1, mid_expr, <<" case Foo of ">>},
            {1, mid_expr, <<" true -> ">>},
            {1, text, <<"<li>">>},
            {1, mid_expr, <<" foo; ">>},
            {1, text, <<"</li>">>},
            {1, mid_expr, <<" Bar -> ">>},
            {2, expr, <<"#">>, <<" Maybe a comment ">>},
            {1, text, <<"<li>">>},
            {1, mid_expr, <<" Bar ">>},
            {2, expr, <<"=">>, <<" Baz. ">>},
            {1, text, <<"</li>">>},
            {1, mid_expr, <<" end ">>},
            {1, end_expr, <<" end, List). ">>},
            {0, text, <<"</ul>">>}
        ],
        scan(
            <<
                "<ul>"
                "<%= lists:map(fun(Foo) -> %>"
                "<% case Foo of %>"
                "<% true -> %>"
                "<li><% foo; %></li>"
                "<% Bar -> %>"
                "<%# Maybe a comment =%>"
                "<li><% Bar %><%= Baz. =%></li>"
                "<% end %>"
                "<% end, List). =%>"
                "</ul>"
            >>
        )
    ).

-endif.
