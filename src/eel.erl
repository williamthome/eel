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

-type symbol() :: text | expr | start_expr | mid_expr | end_expr.
-type marker() :: binary().
-type syntax() :: binary().
-type token() ::
    {text, syntax()}
    | {expr, marker(), syntax()}
    | {start_expr, marker(), syntax()}
    | {mid_expr, syntax()}
    | {end_expr, syntax()}.
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
    scan(Bin, []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec scan(binary(), tokens()) -> tokens().

scan(<<>>, Tokens) ->
    lists:reverse(Tokens);
scan(<<"<%", T/binary>>, Tokens) ->
    {Token, Rest} = guess_token(T),
    scan(Rest, [Token | Tokens]);
scan(Bin, Tokens) ->
    {Syntax, Rest} = guess_syntax(Bin),
    Token = {text, Syntax},
    scan(Rest, [Token | Tokens]).

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

-spec guess_token(binary()) -> {token(), binary()}.

guess_token(Bin) ->
    {Syntax, Rest} = guess_syntax(Bin),
    First = binary:first(Syntax),
    Last = binary:last(Syntax),
    Space = 32,
    Token =
        case Last =:= $= of
            true ->
                case First =:= Space of
                    true ->
                        {end_expr, bin_drop_last(Syntax)};
                    false ->
                        Marker = byte_to_binary(First),
                        {expr, Marker, bin_drop_first_and_last(Syntax)}
                end;
            false ->
                case First =:= Space of
                    true ->
                        {mid_expr, Syntax};
                    false ->
                        Marker = byte_to_binary(First),
                        {start_expr, Marker, bin_drop_first(Syntax)}
                end
        end,
    {Token, Rest}.

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
