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

-type depth() :: non_neg_integer().
-type depths() :: [depth()].
-type symbol() :: text | expr | start_expr | mid_expr | end_expr.
-type marker() :: binary().
-type syntax() :: binary().
-type struct() :: {symbol(), marker(), syntax()}.
-type token() :: {depth(), struct()}.
-type tokens() :: [token()].

-export_type([
    depth/0,
    depths/0,
    symbol/0,
    marker/0,
    syntax/0,
    struct/0,
    token/0,
    tokens/0
]).

-export([
    scan/1,
    sort/1,
    group_by_depth/1,
    gen_text_struct/1,
    gen_expr_struct/2,
    gen_start_expr_struct/2,
    gen_mid_expr_struct/1,
    gen_end_expr_struct/1,
    gen_text_token/2,
    gen_expr_token/3,
    gen_start_expr_token/3,
    gen_mid_expr_token/2,
    gen_end_expr_token/2
]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Scans characters to tokens.
%% @end
%%------------------------------------------------------------------------------
-spec scan(binary()) -> tokens().

scan(Bin) ->
    scan([0], 0, Bin, []).

%%------------------------------------------------------------------------------
%% @doc Sorts tokens from deeper to less deep.
%% @end
%%------------------------------------------------------------------------------
-spec sort(tokens()) -> tokens().

sort(Tokens) ->
    lists:sort(fun depth_compare/2, Tokens).

%%------------------------------------------------------------------------------
%% @doc Group tokens by depth.
%% @end
%%------------------------------------------------------------------------------
-spec group_by_depth(tokens()) -> #{depth() => tokens()}.

group_by_depth(Tokens) ->
    group_by(fun({Depth, _Struct}) -> Depth end, Tokens).

%%%-----------------------------------------------------------------------------
%%% Struct generators
%%%-----------------------------------------------------------------------------

-spec gen_text_struct(syntax()) -> struct().

gen_text_struct(Syntax) ->
    gen_struct(text, <<>>, Syntax).

-spec gen_expr_struct(marker(), syntax()) -> struct().

gen_expr_struct(Marker, Syntax) ->
    gen_struct(expr, Marker, Syntax).

-spec gen_start_expr_struct(marker(), syntax()) -> struct().

gen_start_expr_struct(Marker, Syntax) ->
    gen_struct(start_expr, Marker, Syntax).

-spec gen_mid_expr_struct(syntax()) -> struct().

gen_mid_expr_struct(Syntax) ->
    gen_struct(mid_expr, <<>>, Syntax).

-spec gen_end_expr_struct(syntax()) -> struct().

gen_end_expr_struct(Syntax) ->
    gen_struct(end_expr, <<>>, Syntax).

%%%-----------------------------------------------------------------------------
%%% Token generators
%%%-----------------------------------------------------------------------------

-spec gen_text_token(depth(), syntax()) -> token().

gen_text_token(Depth, Syntax) ->
    Struct = gen_text_struct(Syntax),
    gen_token(Depth, Struct).

-spec gen_expr_token(depth(), marker(), syntax()) -> token().

gen_expr_token(Depth, Marker, Syntax) ->
    Struct = gen_expr_struct(Marker, Syntax),
    gen_token(Depth, Struct).

-spec gen_start_expr_token(depth(), marker(), syntax()) -> token().

gen_start_expr_token(Depth, Marker, Syntax) ->
    Struct = gen_start_expr_struct(Marker, Syntax),
    gen_token(Depth, Struct).

-spec gen_mid_expr_token(depth(), syntax()) -> token().

gen_mid_expr_token(Depth, Syntax) ->
    Struct = gen_mid_expr_struct(Syntax),
    gen_token(Depth, Struct).

-spec gen_end_expr_token(depth(), syntax()) -> token().

gen_end_expr_token(Depth, Syntax) ->
    Struct = gen_end_expr_struct(Syntax),
    gen_token(Depth, Struct).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec scan(depths(), non_neg_integer(), binary(), tokens()) -> tokens().

scan(_Depths, _ExprCount, <<>>, Tokens) ->
    lists:reverse(Tokens);
scan(Depths, ExprCount, <<"<%", T/binary>>, Tokens) ->
    {NewDepths, NewExprCount, NewToken, Rest} =
        guess_token(Depths, ExprCount, T),
    scan(NewDepths, NewExprCount, Rest, [NewToken | Tokens]);
scan([Depth | _] = Depths, ExprCount, Bin, Tokens) ->
    {Syntax, Rest} = guess_syntax(Bin),
    Token = gen_text_token(Depth, Syntax),
    scan(Depths, ExprCount, Rest, [Token | Tokens]).

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

-spec guess_token(depths(), non_neg_integer(), binary()) ->
    {depths(), non_neg_integer(), token(), binary()}.

guess_token([Depth | LessDeep] = AllDepths, ExprCount, Bin) ->
    {Syntax, Rest} = guess_syntax(Bin),
    FirstByte = binary:first(Syntax),
    LastByte = binary:last(Syntax),
    Space = 32,
    {NewDepths, NewExprCount, NewToken} =
        case LastByte =:= $= of
            true ->
                case FirstByte =:= Space of
                    true ->
                        NewSyntax = bin_drop_last(Syntax),
                        Token = gen_end_expr_token(Depth, NewSyntax),
                        {LessDeep, ExprCount, Token};
                    false ->
                        Marker = byte_to_binary(FirstByte),
                        NewSyntax = bin_drop_first_and_last(Syntax),
                        Token = gen_expr_token(ExprCount + 1, Marker, NewSyntax),
                        {AllDepths, ExprCount + 1, Token}
                end;
            false ->
                case FirstByte =:= Space of
                    true ->
                        Token = gen_mid_expr_token(Depth, Syntax),
                        {AllDepths, ExprCount, Token};
                    false ->
                        Deeper = ExprCount + 1,
                        Marker = byte_to_binary(FirstByte),
                        NewSyntax = bin_drop_first(Syntax),
                        Token = gen_start_expr_token(
                            ExprCount + 1, Marker, NewSyntax
                        ),
                        {[Deeper | AllDepths], ExprCount + 1, Token}
                end
        end,
    {NewDepths, NewExprCount, NewToken, Rest}.

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

-spec gen_struct(symbol(), marker(), syntax()) -> struct().

gen_struct(Symbol, Marker, Syntax) ->
    {Symbol, Marker, Syntax}.

-spec gen_token(depth(), struct()) -> token().

gen_token(Depth, Struct) ->
    {Depth, Struct}.

-spec depth_compare(token(), token()) -> boolean().

depth_compare({ADepth, _AStruct}, {BDepth, _BStruct}) ->
    ADepth >= BDepth.

-spec group_by(fun((Elem :: T) -> Key), [T]) -> #{Key => [T]}.

group_by(Fun, List) when is_function(Fun, 1) ->
    Reversed = lists:reverse(List),
    do_group_by(Fun, Reversed, #{}).

-spec do_group_by(fun((Elem :: T) -> Key), [T], #{Key => [T]}) -> #{Key => [T]}.

do_group_by(Fun, [Elem | Tail], AccIn) ->
    Key = Fun(Elem),
    AccOut =
        case AccIn of
            #{Key := Elems} -> AccIn#{Key := [Elem | Elems]};
            #{} -> AccIn#{Key => [Elem]}
        end,
    do_group_by(Fun, Tail, AccOut);
do_group_by(_Fun, [], Acc) ->
    Acc.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

mock_tokens() ->
    [
        {0, {text, <<>>, <<"<ul>">>}},
        {1, {start_expr, <<"=">>, <<" lists:map(fun(Foo) -> ">>}},
        {1, {mid_expr, <<>>, <<" case Foo of ">>}},
        {1, {mid_expr, <<>>, <<" true -> ">>}},
        {1, {text, <<>>, <<"<li>">>}},
        {1, {mid_expr, <<>>, <<" foo; ">>}},
        {1, {text, <<>>, <<"</li>">>}},
        {1, {mid_expr, <<>>, <<" Bar -> ">>}},
        {2, {expr, <<"#">>, <<" Maybe a comment ">>}},
        {1, {text, <<>>, <<"<li>">>}},
        {1, {mid_expr, <<>>, <<" Bar ">>}},
        {3, {expr, <<"=">>, <<" Baz. ">>}},
        {1, {text, <<>>, <<"</li><ul>">>}},
        {4, {start_expr, <<"=">>, <<" lists:map(fun(Foo) -> ">>}},
        {4, {mid_expr, <<>>, <<" case Foo of ">>}},
        {4, {mid_expr, <<>>, <<" true -> ">>}},
        {4, {text, <<>>, <<"<li>">>}},
        {4, {mid_expr, <<>>, <<" foo; ">>}},
        {4, {text, <<>>, <<"</li>">>}},
        {4, {mid_expr, <<>>, <<" Bar -> ">>}},
        {5, {expr, <<"#">>, <<" Maybe a comment ">>}},
        {4, {text, <<>>, <<"<li>">>}},
        {4, {mid_expr, <<>>, <<" Bar ">>}},
        {6, {expr, <<"=">>, <<" Baz. ">>}},
        {4, {text, <<>>, <<"</li>">>}},
        {4, {mid_expr, <<>>, <<" end ">>}},
        {4, {end_expr, <<>>, <<" end, List). ">>}},
        {1, {text, <<>>, <<"</ul>">>}},
        {1, {mid_expr, <<>>, <<" end ">>}},
        {1, {end_expr, <<>>, <<" end, List). ">>}},
        {0, {text, <<>>, <<"</ul>">>}}
    ].

scan_test() ->
    ?assertEqual(
        mock_tokens(),
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
                "<% end %>"
                "<% end, List). =%>"
                "</ul>"
            >>
        )
    ).

sort_test() ->
    ?assertEqual(
        [
            {6, {expr, <<"=">>, <<" Baz. ">>}},
            {5, {expr, <<"#">>, <<" Maybe a comment ">>}},
            {4, {start_expr, <<"=">>, <<" lists:map(fun(Foo) -> ">>}},
            {4, {mid_expr, <<>>, <<" case Foo of ">>}},
            {4, {mid_expr, <<>>, <<" true -> ">>}},
            {4, {text, <<>>, <<"<li>">>}},
            {4, {mid_expr, <<>>, <<" foo; ">>}},
            {4, {text, <<>>, <<"</li>">>}},
            {4, {mid_expr, <<>>, <<" Bar -> ">>}},
            {4, {text, <<>>, <<"<li>">>}},
            {4, {mid_expr, <<>>, <<" Bar ">>}},
            {4, {text, <<>>, <<"</li>">>}},
            {4, {mid_expr, <<>>, <<" end ">>}},
            {4, {end_expr, <<>>, <<" end, List). ">>}},
            {3, {expr, <<"=">>, <<" Baz. ">>}},
            {2, {expr, <<"#">>, <<" Maybe a comment ">>}},
            {1, {start_expr, <<"=">>, <<" lists:map(fun(Foo) -> ">>}},
            {1, {mid_expr, <<>>, <<" case Foo of ">>}},
            {1, {mid_expr, <<>>, <<" true -> ">>}},
            {1, {text, <<>>, <<"<li>">>}},
            {1, {mid_expr, <<>>, <<" foo; ">>}},
            {1, {text, <<>>, <<"</li>">>}},
            {1, {mid_expr, <<>>, <<" Bar -> ">>}},
            {1, {text, <<>>, <<"<li>">>}},
            {1, {mid_expr, <<>>, <<" Bar ">>}},
            {1, {text, <<>>, <<"</li><ul>">>}},
            {1, {text, <<>>, <<"</ul>">>}},
            {1, {mid_expr, <<>>, <<" end ">>}},
            {1, {end_expr, <<>>, <<" end, List). ">>}},
            {0, {text, <<>>, <<"<ul>">>}},
            {0, {text, <<>>, <<"</ul>">>}}
        ],
        sort(mock_tokens())
    ).

group_by_depth_test() ->
    ?assertEqual(
        #{
            0 => [
                {0, {text, <<>>, <<"<ul>">>}},
                {0, {text, <<>>, <<"</ul>">>}}
            ],
            1 => [
                {1, {start_expr, <<"=">>, <<" lists:map(fun(Foo) -> ">>}},
                {1, {mid_expr, <<>>, <<" case Foo of ">>}},
                {1, {mid_expr, <<>>, <<" true -> ">>}},
                {1, {text, <<>>, <<"<li>">>}},
                {1, {mid_expr, <<>>, <<" foo; ">>}},
                {1, {text, <<>>, <<"</li>">>}},
                {1, {mid_expr, <<>>, <<" Bar -> ">>}},
                {1, {text, <<>>, <<"<li>">>}},
                {1, {mid_expr, <<>>, <<" Bar ">>}},
                {1, {text, <<>>, <<"</li><ul>">>}},
                {1, {text, <<>>, <<"</ul>">>}},
                {1, {mid_expr, <<>>, <<" end ">>}},
                {1, {end_expr, <<>>, <<" end, List). ">>}}
            ],
            2 => [
                {2, {expr, <<"#">>, <<" Maybe a comment ">>}}
            ],
            3 => [
                {3, {expr, <<"=">>, <<" Baz. ">>}}
            ],
            4 => [
                {4, {start_expr, <<"=">>, <<" lists:map(fun(Foo) -> ">>}},
                {4, {mid_expr, <<>>, <<" case Foo of ">>}},
                {4, {mid_expr, <<>>, <<" true -> ">>}},
                {4, {text, <<>>, <<"<li>">>}},
                {4, {mid_expr, <<>>, <<" foo; ">>}},
                {4, {text, <<>>, <<"</li>">>}},
                {4, {mid_expr, <<>>, <<" Bar -> ">>}},
                {4, {text, <<>>, <<"<li>">>}},
                {4, {mid_expr, <<>>, <<" Bar ">>}},
                {4, {text, <<>>, <<"</li>">>}},
                {4, {mid_expr, <<>>, <<" end ">>}},
                {4, {end_expr, <<>>, <<" end, List). ">>}}
            ],
            5 => [
                {5, {expr, <<"#">>, <<" Maybe a comment ">>}}
            ],
            6 => [
                {6, {expr, <<"=">>, <<" Baz. ">>}}
            ]
        },
        group_by_depth(mock_tokens())
    ).

-endif.
