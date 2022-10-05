-module(eel_tokenizer).

%% API functions
-export([
    tokenize/1,
    tokenize/2
]).

%% Includes
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([ handle_expr/2 ]).
-endif.

%% Defines
-define(DEFAULT_ENGINE, eel_smart_engine).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec tokenize(binary()) -> {ok, list()} | {error, term()}.

tokenize(Bin) ->
    tokenize(Bin, ?DEFAULT_ENGINE).

-spec tokenize(binary(), module()) -> {ok, list()} | {error, term()}.

tokenize(Bin, Eng) ->
    do_tokenize(Bin, in_root, Eng, {1, 1}, {<<>>, 1, 1}, {<<>>, <<>>}, <<>>, []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_tokenize(<<$<, T/binary>>, in_root, Eng, {Ln, Col}, {<<>>, SLn, SCol}, {<<>>, <<>>}, Buf, Acc) ->
    do_tokenize(
        T,
        in_marker,
        Eng,
        {Ln, Col + 1},
        {<<>>, SLn, SCol},
        {<<"<">>, <<>>},
        <<Buf/binary, "<">>,
        Acc
    );
do_tokenize(<<$<, T/binary>>, in_root, Eng, {Ln, Col}, {<<>>, SLn, SCol}, {Exp, Exp}, Buf, Acc0) ->
    case Eng:handle_expr({{SLn, SCol}, {<<>>, <<>>}, {Exp, Exp}}, Acc0) of
        {ok, Acc} ->
            do_tokenize(<<$<, T/binary>>, in_root, Eng, {Ln, Col}, {<<>>, Ln, Col}, {<<>>, <<>>}, Buf, Acc);
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(
    <<32, T/binary>>, in_marker, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, Acc
) ->
    do_tokenize(
        T,
        in_expr,
        Eng,
        {Ln, Col + 1},
        {SMkr, SLn, SCol},
        {<<ExpOut/binary, 32>>, ExpIn},
        <<Buf/binary, 32>>,
        Acc
    );
do_tokenize(
    <<$>, T/binary>>, in_marker, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, Acc0
) ->
    EMkr = <<>>,
    case Eng:handle_expr({{SLn, SCol}, {SMkr, EMkr}, {<<ExpOut/binary, ">">>, ExpIn}}, Acc0) of
        {ok, Acc} ->
            do_tokenize(
                T, in_root, Eng, {Ln, Col + 1}, {<<>>, Ln, Col + 1}, {<<>>, <<>>}, <<Buf/binary, ">">>, Acc
            );
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(
    <<32, T/binary>>, in_expr, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, Acc0
) ->
    case is_end_of_expr(T) of
        {true, {Rest, EMkr}} ->
            case Eng:handle_expr({{SLn, SCol}, {SMkr, EMkr}, {<<ExpOut/binary, 32, EMkr/binary>>, ExpIn}}, Acc0) of
                {ok, Acc} ->
                    do_tokenize(
                        Rest,
                        in_root,
                        Eng,
                        {Ln, Col + 1 + size(EMkr) + 1},
                        {<<>>, Ln, Col + 1 + size(EMkr) + 1},
                        {<<>>, <<>>},
                        <<Buf/binary, 32, EMkr/binary, ">">>,
                        Acc
                    );
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            do_tokenize(
                T,
                in_expr,
                Eng,
                {Ln, Col + 1},
                {SMkr, SLn, SCol},
                {<<ExpOut/binary, 32>>, ExpIn},
                <<Buf/binary, 32>>,
                Acc0
            );
        eof ->
            {error, eof}
    end;
do_tokenize(
    <<H, T/binary>>, in_marker, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, Acc
) ->
    do_tokenize(
        T,
        in_marker,
        Eng,
        {Ln, Col + 1},
        {<<SMkr/binary, H>>, SLn, SCol},
        {<<ExpOut/binary, H>>, ExpIn},
        <<Buf/binary, H>>,
        Acc
    );
do_tokenize(
    <<$>, T/binary>>, in_expr, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, Acc0
) ->
    EMkr = ExpIn,
    case Eng:handle_expr({{SLn, SCol}, {SMkr, EMkr}, {<<ExpOut/binary, ">">>, <<>>}}, Acc0) of
        {ok, Acc} ->
            do_tokenize(
                T, in_root, Eng, {Ln, Col + 1}, {<<>>, Ln, Col + 1}, {<<>>, <<>>}, <<Buf/binary, ">">>, Acc
            );
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(<<H, T/binary>>, in_expr, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, Acc) ->
    do_tokenize(
        T,
        in_expr,
        Eng,
        {Ln, Col + 1},
        {SMkr, SLn, SCol},
        {<<ExpOut/binary, H>>, <<ExpIn/binary, H>>},
        <<Buf/binary, H>>,
        Acc
    );
do_tokenize(
    <<"\n", T/binary>>, In, Eng, {Ln, _Col}, {SMkr, _SLn, _SCol}, {ExpOut, ExpIn}, Buf, Acc
) ->
    do_tokenize(T, In, Eng, {Ln + 1, 1}, {SMkr, Ln + 1, 1}, {ExpOut, ExpIn}, Buf, Acc);
do_tokenize(<<H, T/binary>>, In, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, Acc) ->
    do_tokenize(
        T,
        In,
        Eng,
        {Ln, Col + 1},
        {SMkr, SLn, SCol},
        {<<ExpOut/binary, H>>, <<ExpIn/binary, H>>},
        <<Buf/binary, H>>,
        Acc
    );
do_tokenize(<<>>, in_root, _Eng, {_Ln, _Col}, {<<>>, _SLn, _SCol}, {<<>>, <<>>}, _Buf, Acc) ->
    {ok, lists:reverse(Acc)};
do_tokenize(<<>>, _In, _Eng, {_Ln, _Col}, {_SMkr, _SLn, _SCol}, {_ExpOut, _ExpIn}, _Buf, _Acc) ->
    {error, eof}.

is_end_of_expr(Bin) ->
    is_end_of_expr(Bin, <<>>).

is_end_of_expr(<<$>, T/binary>>, Marker) ->
    {true, {T, Marker}};
is_end_of_expr(<<32, _/binary>>, _) ->
    false;
is_end_of_expr(<<$<, _/binary>>, _) ->
    false;
is_end_of_expr(<<H, T/binary>>, Acc) ->
    is_end_of_expr(T, <<Acc/binary, H>>);
is_end_of_expr(<<>>, _) ->
    eof.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

tokenize_test() ->
    Bin = <<"<html>\n    <%= .%>Foo<%= Bar .%>\n</html>">>,
    Expected =
        {ok, [
            {{1, 1}, {<<"html">>, <<>>}, {<<"<html>">>, <<>>}},
            {{2, 1}, {<<>>, <<>>}, {<<"    ">>, <<"    ">>}},
            {{2, 5}, {<<"%=">>, <<".%">>}, {<<"<%= .%>">>, <<>>}},
            {{2, 12}, {<<>>, <<>>}, {<<"Foo">>, <<"Foo">>}},
            {{2, 15}, {<<"%=">>, <<".%">>}, {<<"<%= Bar .%">>, <<"Bar">>}},
            {{3, 1}, {<<"/html">>, <<>>}, {<<"</html>">>, <<>>}}
        ]},
    ?assertEqual(Expected, tokenize(Bin, ?MODULE)).

% Engine

handle_expr({{Ln, Col}, {SMkr, EMkr}, {ExpOut, ExpIn}}, Acc) ->
    {ok, [{{Ln, Col}, {SMkr, EMkr}, {ExpOut, ExpIn}} | Acc]}.

-endif.
