-module(eel_tokenizer).

%% API functions
-export([
    tokenize/1,
    tokenize/3
]).

%% Includes
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([init/1, handle_expr/2, handle_text/2, handle_body/1]).
-endif.

%% Defines
-define(DEFAULT_ENGINE, eel_smart_engine).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec tokenize(binary()) -> {ok, list()} | {error, term()}.

tokenize(Bin) ->
    tokenize(Bin, ?DEFAULT_ENGINE, []).

-spec tokenize(binary(), module(), term()) -> {ok, list()} | {error, term()}.

tokenize(Bin, Eng, Opts) ->
    case Eng:init(Opts) of
        {ok, State} ->
            do_tokenize(Bin, in_root, Eng, {1, 1}, {<<>>, 1, 1}, {<<>>, <<>>}, <<>>, State);
        {error, Reason} ->
            {error, Reason}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_tokenize(
    <<"<%", T/binary>>, in_root, Eng, {Ln, Col}, {<<>>, SLn, SCol}, {<<>>, <<>>}, Buf, State
) ->
    do_tokenize(
        T,
        in_marker,
        Eng,
        {Ln, Col + 2},
        {<<>>, SLn, SCol},
        {<<"<%">>, <<>>},
        <<Buf/binary, "<%">>,
        State
    );
do_tokenize(
    <<"<%", T/binary>>, in_root, Eng, {Ln, Col}, {<<>>, SLn, SCol}, {Text, Text}, Buf, State0
) ->
    case Eng:handle_text({{SLn, SCol}, Text}, State0) of
        {ok, State} ->
            do_tokenize(
                <<"<%", T/binary>>,
                in_root,
                Eng,
                {Ln, Col},
                {<<>>, Ln, Col},
                {<<>>, <<>>},
                Buf,
                State
            );
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(
    <<32, T/binary>>, in_marker, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State
) ->
    do_tokenize(
        T,
        in_expr,
        Eng,
        {Ln, Col + 1},
        {SMkr, SLn, SCol},
        {<<ExpOut/binary, 32>>, ExpIn},
        <<Buf/binary, 32>>,
        State
    );
do_tokenize(
    <<"%>", T/binary>>, in_marker, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State0
) ->
    EMkr = <<>>,
    case Eng:handle_expr({{SLn, SCol}, {SMkr, EMkr}, {<<ExpOut/binary, "%>">>, ExpIn}}, State0) of
        {ok, State} ->
            do_tokenize(
                T,
                in_root,
                Eng,
                {Ln, Col + 2},
                {<<>>, Ln, Col + 2},
                {<<>>, <<>>},
                <<Buf/binary, "%>">>,
                State
            );
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(
    <<32, T/binary>>, in_expr, Eng, {Ln0, Col0}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State0
) ->
    case is_end_of_expr(T, {Ln0, Col0 + 1}) of
        {true, {Rest, {Ln, Col}, EMkr}} ->
            case
                Eng:handle_expr(
                    {{SLn, SCol}, {SMkr, EMkr},
                     {<<ExpOut/binary, 32, EMkr/binary, "%>">>, ExpIn}},
                    State0
                )
            of
                {ok, State} ->
                    do_tokenize(
                        Rest,
                        in_root,
                        Eng,
                        {Ln, Col},
                        {<<>>, Ln, Col},
                        {<<>>, <<>>},
                        <<Buf/binary, 32, EMkr/binary, "%>">>,
                        State
                    );
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            do_tokenize(
                T,
                in_expr,
                Eng,
                {Ln0, Col0 + 1},
                {SMkr, SLn, SCol},
                {<<ExpOut/binary, 32>>, <<ExpIn/binary, 32>>},
                <<Buf/binary, 32>>,
                State0
            );
        eof ->
            {error, eof}
    end;
do_tokenize(
    <<H, T/binary>>, in_marker, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State
) ->
    do_tokenize(
        T,
        in_marker,
        Eng,
        {Ln, Col + 1},
        {<<SMkr/binary, H>>, SLn, SCol},
        {<<ExpOut/binary, H>>, ExpIn},
        <<Buf/binary, H>>,
        State
    );
do_tokenize(
    <<"%>", T/binary>>, in_expr, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State0
) ->
    EMkr = ExpIn,
    case Eng:handle_expr({{SLn, SCol}, {SMkr, EMkr}, {<<ExpOut/binary, "%>">>, <<>>}}, State0) of
        {ok, State} ->
            do_tokenize(
                T,
                in_root,
                Eng,
                {Ln, Col + 2},
                {<<>>, Ln, Col + 2},
                {<<>>, <<>>},
                <<Buf/binary, "%>">>,
                State
            );
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(
    <<"\n", T/binary>>, in_root, Eng, {Ln, _Col}, {<<>>, SLn, SCol}, {Text, Text}, Buf, State0
) ->
    case Eng:handle_text({{SLn, SCol}, Text}, State0) of
        {ok, State} ->
            do_tokenize(
                T,
                in_root,
                Eng,
                {Ln + 1, 1},
                {<<>>, SLn + 1, 1},
                {<<>>, <<>>},
                Buf,
                State
            );
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(
    <<"\n", T/binary>>, in_expr, Eng, {Ln, _Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State
) ->
    do_tokenize(T, in_expr, Eng, {Ln + 1, 1}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State);
do_tokenize(<<H, T/binary>>, In, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State) ->
    do_tokenize(
        T,
        In,
        Eng,
        {Ln, Col + 1},
        {SMkr, SLn, SCol},
        {<<ExpOut/binary, H>>, <<ExpIn/binary, H>>},
        <<Buf/binary, H>>,
        State
    );
do_tokenize(<<>>, in_root, Eng, {_Ln, _Col}, {<<>>, _SLn, _SCol}, {<<>>, <<>>}, _Buf, State) ->
    Eng:handle_body(State);
do_tokenize(<<>>, in_root, Eng, {Ln, Col}, {<<>>, SLn, SCol}, {Text, Text}, Buf, State0) ->
    case Eng:handle_text({{SLn, SCol}, Text}, State0) of
        {ok, State} ->
            do_tokenize(
                <<>>,
                in_root,
                Eng,
                {Ln, Col},
                {<<>>, Ln, Col},
                {<<>>, <<>>},
                Buf,
                State
            );
        {error, Reason} ->
            {error, Reason}
    end;
do_tokenize(<<>>, _In, _Eng, {_Ln, _Col}, {_SMkr, _SLn, _SCol}, {_ExpOut, _ExpIn}, _Buf, _State) ->
    {error, eof}.

is_end_of_expr(Bin, Cursor) ->
    is_end_of_expr(Bin, Cursor, <<>>).

is_end_of_expr(<<"%>", T/binary>>, {Ln, Col}, Marker) ->
    {true, {T, {Ln, Col + 2}, Marker}};
is_end_of_expr(<<32, _/binary>>, _, _) ->
    false;
is_end_of_expr(<<"<%", _/binary>>, _, _) ->
    false;
is_end_of_expr(<<"\n", T/binary>>, {Ln, _Col}, Marker) ->
    is_end_of_expr(T, {Ln + 1, 1}, Marker);
is_end_of_expr(<<H, T/binary>>, {Ln, Col}, Marker) ->
    is_end_of_expr(T, {Ln, Col + 1}, <<Marker/binary, H>>);
is_end_of_expr(<<>>, _, _) ->
    eof.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

tokenize_test() ->
    Bin = <<"<html>\n    <%= .%>Foo<%= Bar \n.%>     \n</html>">>,
    Expected =
        {ok, [
            {text, {{1, 1}, <<"<html>">>}},
            {text, {{2, 1}, <<"    ">>}},
            {expr, {{2, 5}, {<<"=">>, <<".">>}, {<<"<%= .%>">>, <<>>}}},
            {text, {{2, 12}, <<"Foo">>}},
            {expr, {{2, 15}, {<<"=">>, <<".">>}, {<<"<%= Bar .%>">>, <<"Bar">>}}},
            {text, {{3, 4}, <<"     ">>}},
            {text, {{4, 1}, <<"</html>">>}}
        ]},
    ?assertEqual(Expected, tokenize(Bin, ?MODULE, [])).

% Engine

init([]) ->
    {ok, []}.

handle_expr({{Ln, Col}, {SMkr, EMkr}, {ExpOut, ExpIn}}, Acc) ->
    {ok, [{expr, {{Ln, Col}, {SMkr, EMkr}, {ExpOut, ExpIn}}} | Acc]}.

handle_text({{Ln, Col}, Text}, Acc) ->
    {ok, [{text, {{Ln, Col}, Text}} | Acc]}.

handle_body(Tokens) ->
    {ok, lists:reverse(Tokens)}.

-endif.
