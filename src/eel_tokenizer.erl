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
        {ok, {StartMarker, EndMarker, State}} ->
            do_tokenize(StartMarker, EndMarker, Bin, in_root, Eng, {1, 1}, {<<>>, 1, 1}, {<<>>, <<>>}, <<>>, State);
        {error, Reason} ->
            {error, Reason}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_tokenize(StartMarker, EndMarker, Bin, In, Eng, Pos, Acc, Expr, Buf, State) ->
    case size(Bin) > length(StartMarker) andalso binary:split(Bin, list_to_binary(StartMarker), [{scope, {0, length(StartMarker)}}]) of
        [<<>>, TStart] ->
            handle_start_marker(StartMarker, EndMarker, TStart, In, Eng, Pos, Acc, Expr, Buf, State);
        _ ->
            case size(Bin) > length(EndMarker) andalso binary:split(Bin, list_to_binary(EndMarker), [{scope, {0, length(EndMarker)}}]) of
                [<<>>, TEnd] ->
                    handle_end_marker(StartMarker, EndMarker, TEnd, In, Eng, Pos, Acc, Expr, Buf, State);
                _ ->
                    handle_text(StartMarker, EndMarker, Bin, In, Eng, Pos, Acc, Expr, Buf, State)
            end
    end.

handle_start_marker(StartMarker, EndMarker, T, in_root, Eng, {Ln, Col}, {<<>>, SLn, SCol}, {<<>>, <<>>}, Buf, State) ->
    do_tokenize(
        StartMarker,
        EndMarker,
        T,
        in_marker,
        Eng,
        {Ln, Col + length(StartMarker)},
        {<<>>, SLn, SCol},
        {list_to_binary(StartMarker), <<>>},
        iolist_to_binary([Buf, StartMarker]),
        State
    );
handle_start_marker(StartMarker, EndMarker, T, in_root, Eng, {Ln, Col}, {<<>>, SLn, SCol}, {Text, Text}, Buf, State0) ->
    case Eng:handle_text({{SLn, SCol}, Text}, State0) of
        {ok, State} ->
            do_tokenize(
                StartMarker,
                EndMarker,
                iolist_to_binary([StartMarker, T]),
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
    end.

handle_end_marker(StartMarker, EndMarker, T, in_marker, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State0) ->
    EMkr = <<>>,
    case Eng:handle_expr({{SLn, SCol}, {SMkr, EMkr}, {iolist_to_binary([ExpOut, EndMarker]), ExpIn}}, State0) of
        {ok, State} ->
            do_tokenize(
                StartMarker,
                EndMarker,
                T,
                in_root,
                Eng,
                {Ln, Col + length(EndMarker)},
                {<<>>, Ln, Col + length(EndMarker)},
                {<<>>, <<>>},
                iolist_to_binary([Buf, EndMarker]),
                State
            );
        {error, Reason} ->
            {error, Reason}
    end;
handle_end_marker(StartMarker, EndMarker, T, in_expr, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State0) ->
    EMkr = ExpIn,
    case Eng:handle_expr({{SLn, SCol}, {SMkr, EMkr}, {iolist_to_binary([ExpOut, EndMarker]), <<>>}}, State0) of
        {ok, State} ->
            do_tokenize(
                StartMarker,
                EndMarker,
                T,
                in_root,
                Eng,
                {Ln, Col + length(EndMarker)},
                {<<>>, Ln, Col + length(EndMarker)},
                {<<>>, <<>>},
                iolist_to_binary([Buf, EndMarker]),
                State
            );
        {error, Reason} ->
            {error, Reason}
    end.

handle_text(StartMarker, EndMarker, <<32, T/binary>>, in_marker, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State) ->
    do_tokenize(
        StartMarker,
        EndMarker,
        T,
        in_expr,
        Eng,
        {Ln, Col + 1},
        {SMkr, SLn, SCol},
        {<<ExpOut/binary, 32>>, ExpIn},
        <<Buf/binary, 32>>,
        State
    );
handle_text(StartMarker, EndMarker, <<32, T/binary>>, in_expr, Eng, {Ln0, Col0}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State0) ->
    case is_end_of_expr(StartMarker, EndMarker, T, {Ln0, Col0 + 1}, <<>>) of
        {true, {Rest, {Ln, Col}, EMkr}} ->
            case
                Eng:handle_expr(
                    {{SLn, SCol}, {SMkr, EMkr},
                     {iolist_to_binary([ExpOut, 32, EMkr, EndMarker]), ExpIn}},
                    State0
                )
            of
                {ok, State} ->
                    do_tokenize(
                        StartMarker,
                        EndMarker,
                        Rest,
                        in_root,
                        Eng,
                        {Ln, Col},
                        {<<>>, Ln, Col},
                        {<<>>, <<>>},
                        iolist_to_binary([Buf, 32, EMkr, EndMarker]),
                        State
                    );
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            do_tokenize(
                StartMarker,
                EndMarker,
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
handle_text(StartMarker, EndMarker, <<H, T/binary>>, in_marker, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State) ->
    do_tokenize(
        StartMarker,
        EndMarker,
        T,
        in_marker,
        Eng,
        {Ln, Col + 1},
        {<<SMkr/binary, H>>, SLn, SCol},
        {<<ExpOut/binary, H>>, ExpIn},
        <<Buf/binary, H>>,
        State
    );
handle_text(StartMarker, EndMarker,
    <<"\n", T/binary>>, in_root, Eng, {Ln, _Col}, {<<>>, SLn, SCol}, {Text, Text}, Buf, State0
) ->
    case Eng:handle_text({{SLn, SCol}, Text}, State0) of
        {ok, State} ->
            do_tokenize(
                StartMarker,
                EndMarker,
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
handle_text(StartMarker, EndMarker,
    <<"\n", T/binary>>, in_expr, Eng, {Ln, _Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State
) ->
    do_tokenize(StartMarker, EndMarker, T, in_expr, Eng, {Ln + 1, 1}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State);
handle_text(StartMarker, EndMarker, <<H, T/binary>>, In, Eng, {Ln, Col}, {SMkr, SLn, SCol}, {ExpOut, ExpIn}, Buf, State) ->
    do_tokenize(
        StartMarker,
        EndMarker,
        T,
        In,
        Eng,
        {Ln, Col + 1},
        {SMkr, SLn, SCol},
        {<<ExpOut/binary, H>>, <<ExpIn/binary, H>>},
        <<Buf/binary, H>>,
        State
    );
handle_text(_StartMarker, _EndMarker, <<>>, in_root, Eng, {_Ln, _Col}, {<<>>, _SLn, _SCol}, {<<>>, <<>>}, _Buf, State) ->
    Eng:handle_body(State);
handle_text(StartMarker, EndMarker, <<>>, in_root, Eng, {Ln, Col}, {<<>>, SLn, SCol}, {Text, Text}, Buf, State0) ->
    case Eng:handle_text({{SLn, SCol}, Text}, State0) of
        {ok, State} ->
            do_tokenize(
                StartMarker,
                EndMarker,
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
handle_text(_StartMarker, _EndMarker, <<>>, _In, _Eng, {_Ln, _Col}, {_SMkr, _SLn, _SCol}, {_ExpOut, _ExpIn}, _Buf, _State) ->
    {error, eof}.

is_end_of_expr(StartMarker, EndMarker, Bin, {Ln, Col}, Acc) ->
    case size(Bin) > length(EndMarker) andalso binary:split(Bin, list_to_binary(EndMarker), [{scope, {0, length(EndMarker)}}]) of
        [<<>>, TEnd] ->
            {true, {TEnd, {Ln, Col + length(EndMarker)}, Acc}};
        _ ->
            case size(Bin) > length(StartMarker) andalso binary:split(Bin, list_to_binary(StartMarker), [{scope, {0, length(StartMarker)}}]) of
                [<<>>, _TStart] ->
                    false;
                _ ->
                    case Bin of
                        <<32, _/binary>> ->
                            false;
                        <<"\n", T/binary>> ->
                            is_end_of_expr(StartMarker, EndMarker, T, {Ln + 1, 1}, Acc);
                        <<H, T/binary>> ->
                            is_end_of_expr(StartMarker, EndMarker, T, {Ln, Col + 1}, <<Acc/binary, H>>);
                        <<>> ->
                            eof
                    end
            end
    end.

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
    {ok, {"<%", "%>", []}}.

handle_expr({{Ln, Col}, {SMkr, EMkr}, {ExpOut, ExpIn}}, Acc) ->
    {ok, [{expr, {{Ln, Col}, {SMkr, EMkr}, {ExpOut, ExpIn}}} | Acc]}.

handle_text({{Ln, Col}, Text}, Acc) ->
    {ok, [{text, {{Ln, Col}, Text}} | Acc]}.

handle_body(Tokens) ->
    {ok, lists:reverse(Tokens)}.

-endif.
