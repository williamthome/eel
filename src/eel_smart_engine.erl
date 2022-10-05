-module(eel_smart_engine).

-behaviour(eel_engine).

%% eel_engine callbacks
-export([
    handle_expr/2
]).

%% Includes
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% eel_engine callbacks
%%%=============================================================================

handle_expr({{_Ln, _Col}, {<<"%=">>, <<".%">>}, {_ExpOut, <<>>}}, Acc) ->
    {ok, Acc};
handle_expr({{Ln, Col}, {<<"%=">>, <<".%">>}, {_ExpOut, ExpIn}}, Acc) ->
    {ok, [{{Ln, Col}, {expr, ExpIn}} | Acc]};
handle_expr({{Ln, Col}, {<<"%=">>, <<"%">>}, {_ExpOut, ExpIn}}, Acc) ->
    {ok, [{{Ln, Col}, {start_expr, ExpIn}} | Acc]};
handle_expr({{Ln, Col}, {<<"%">>, <<"%">>}, {_ExpOut, ExpIn}}, Acc) ->
    {ok, [{{Ln, Col}, {mid_expr, ExpIn}} | Acc]};
handle_expr({{Ln, Col}, {<<"%">>, <<".%">>}, {_ExpOut, ExpIn}}, Acc) ->
    {ok, [{{Ln, Col}, {end_expr, ExpIn}} | Acc]};
handle_expr({{Ln, Col}, {<<"%%">>, <<".%">>}, {_ExpOut, ExpIn}}, Acc) ->
    {ok, [{{Ln, Col}, {comment, ExpIn}} | Acc]};
handle_expr({{Ln, Col}, {<<"%", _/binary>> = StartMarker, EndMarker}, {ExpOut, ExpIn}}, Acc) ->
    {error, {unknown_marker, {{Ln, Col}, {StartMarker, EndMarker}, {ExpOut, ExpIn}, Acc}}};
handle_expr({_Cursor, _Markers, {ExpOut, _ExpIn}}, [{{Ln, Col}, {text, TAcc}} | Acc]) ->
    {ok, [{{Ln, Col}, {text, <<TAcc/binary, ExpOut/binary>>}} | Acc]};
handle_expr({{Ln, Col}, _Markers, {ExpOut, _ExpIn}}, Acc) ->
    {ok, [{{Ln, Col}, {text, ExpOut}} | Acc]}.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

handle_expr_test() ->
    [
        {
            "Should return empty if no expression",
            ?assertEqual(
                {ok, []},
                handle_expr({{1, 1}, {<<"%=">>, <<".%">>}, {<<>>, <<>>}}, [])
            )
        },
        {
            "Should return expr token",
            ?assertEqual(
                {ok, [{{1, 1}, {expr, <<"Foo">>}}]},
                handle_expr({{1, 1}, {<<"%=">>, <<".%">>}, {<<"<%= Foo .%>">>, <<"Foo">>}}, [])
            )
        },
        {
            "Should return start_expr token",
            ?assertEqual(
                {ok, [{{1, 1}, {start_expr, <<"Foo">>}}]},
                handle_expr({{1, 1}, {<<"%=">>, <<"%">>}, {<<"<%= Foo %>">>, <<"Foo">>}}, [])
            )
        },
        {
            "Should return mid_expr token",
            ?assertEqual(
                {ok, [{{1, 1}, {mid_expr, <<"Foo">>}}]},
                handle_expr({{1, 1}, {<<"%">>, <<"%">>}, {<<"<% Foo %>">>, <<"Foo">>}}, [])
            )
        },
        {
            "Should return end_expr token",
            ?assertEqual(
                {ok, [{{1, 1}, {end_expr, <<"Foo">>}}]},
                handle_expr({{1, 1}, {<<"%">>, <<".%">>}, {<<"<% Foo .%>">>, <<"Foo">>}}, [])
            )
        },
        {
            "Should return text token",
            ?assertEqual(
                {ok, [{{1, 1}, {text, <<"Foo">>}}]},
                handle_expr({{1, 1}, {<<>>, <<>>}, {<<"Foo">>, <<"Foo">>}}, [])
            )
        },
        {
            "Should concatenate text",
            ?assertEqual(
                {ok, [{{1, 1}, {text, <<"FooBar">>}}]},
                handle_expr(
                    {{1, 1}, {<<>>, <<>>}, {<<"Bar">>, <<"Bar">>}},
                    [{{1, 1}, {text, <<"Foo">>}}]
                )
            )
        },
        {
            "Should return comment token",
            ?assertEqual(
                {ok, [{{1, 1}, {comment, <<"Foo">>}}]},
                handle_expr({{1, 1}, {<<"%%">>, <<".%">>}, {<<"<%% Foo .%>">>, <<"Foo">>}}, [])
            )
        },
        {
            "Should return unknown marker error",
            ?assertEqual(
                {error, {unknown_marker, {{1, 1}, {<<"%.">>, <<".%">>}, {<<"<%. Foo .%>">>, <<"Foo">>}, []}}},
                handle_expr({{1, 1}, {<<"%.">>, <<".%">>}, {<<"<%. Foo .%>">>, <<"Foo">>}}, [])
            )
        }
    ].

-endif.
