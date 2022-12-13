-module(eel_smart_engine).

-behaviour(eel_engine).

%% eel_engine callbacks
-export([
    init/1,
    markers/0,
    handle_expr/4,
    handle_text/3,
    handle_body/1
]).

%% Includes
-include("eel.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Defines
-define(token(Name), fun (Bin) -> {Name, Bin} end).
-define(text,       ?token(text)).
-define(expr,       ?token(expr)).
-define(start_expr, ?token(start_expr)).
-define(mid_expr,   ?token(mid_expr)).
-define(end_expr,   ?token(end_expr)).
-define(comment,    ?token(comment)).
-define(debug,      ?token(debug)).

%% Types
-type token_name() :: text
                      | expr
                      | start_expr
                      | mid_expr
                      | end_expr
                      | comment
                      | debug.
-type token()   :: {token_name(), binary()}.
-type static()  :: list().
-type dynamic() :: list().

%% State
-record(state, {
    tokens = [] :: [token()]
}).

%%%=============================================================================
%%% eel_engine callbacks
%%%=============================================================================

init([]) -> #state{}.

markers() -> [{"<%=", ".%>"},
              {"<%=",  "%>"},
              {"<%",   "%>"},
              {"<%",  ".%>"},
              {"<%:", ":%>"},
              {"<%%", "%%>"}].

handle_expr(_Pos, {"<%=", ".%>"}, Expr, State) ->
    push(?expr(Expr), State);
handle_expr(_Pos, {"<%=", "%>"}, Expr, State) ->
    push(?start_expr(Expr), State);
handle_expr(_Pos, {"<%", "%>"}, Expr, State) ->
    push(?mid_expr(Expr), State);
handle_expr(_Pos, {"<%", ".%>"}, Expr, State) ->
    push(?end_expr(Expr), State);
handle_expr(_Pos, {"<%:", ":%>"}, Expr, State) ->
    push(?debug(Expr), State);
handle_expr(_Pos, {"<%%", "%%>"}, Expr, State) ->
    push(?comment(Expr), State);
handle_expr(Pos, Marker, Expr, _State) ->
    ?unknown_marker_error({Pos, Marker, Expr}).

handle_text(_Pos, Text, State) ->
    push(?text(Text), State).

handle_body(#state{tokens = Tokens}) ->
    _SD = parse_sd(lists:reverse(Tokens)),
    % TODO: Continue parsing tokens to AST
    lists:reverse(Tokens).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

push(Token, #state{tokens = Tokens} = State) ->
    State#state{tokens = [Token | Tokens]}.

%% -----------------------------------------------------------------------------
%% @private
%% @doc Parses tokens to statics and dynamics.
%% @end
%% -----------------------------------------------------------------------------
-spec parse_sd(Tokens :: [token()]) -> {static(), dynamic()}.

parse_sd(Tokens) ->
    parse_sd(Tokens, {[], []}).

parse_sd(Tokens, SD) ->
    case do_parse_sd(Tokens, in_text, SD) of
        {[], {S, D}} ->
            {lists:reverse(S), D};
        {RestTokens, AccSD} ->
            parse_sd(RestTokens, AccSD)
    end.

do_parse_sd([{expr, _} = H | T], In, {S, D}) ->
    do_parse_sd(T, In, {S, [H | D]});
do_parse_sd([{start_expr, _} = H | T], in_text, {S, D}) ->
    do_parse_sd(T, in_expr, {S, [H | D]});
do_parse_sd([{start_expr, _} | _] = T, in_expr, {S, D}) ->
    {Tokens, SD} = do_parse_sd(T, in_text, {[], []}),
    do_parse_sd(Tokens, in_expr, {S, [{nested_expr, SD} | D]});
do_parse_sd([{mid_expr, _} = H | T], in_expr, {S, D}) ->
    do_parse_sd(T, in_expr, {S, [H | D]});
do_parse_sd([{end_expr, _} = H | T], in_expr, {S, D}) ->
    {T, {lists:reverse(S), lists:reverse([H | D])}};
do_parse_sd([{text, _} = H | T], in_text, {S, D}) ->
    do_parse_sd(T, in_text, {[H | S], D});
do_parse_sd([{text, _} = H | T], in_expr, {S, D}) ->
    do_parse_sd(T, in_expr, {S, [H | D]});
do_parse_sd([{comment, _} | T], In, {S, D}) ->
    do_parse_sd(T, In, {S, D});
do_parse_sd([{debug, _} = H | T], In, {S, D}) ->
    do_parse_sd(T, In, {S, [H | D]});
do_parse_sd([], _, SD) ->
    {[], SD}.

% TODO: Retrieve funs vars
% retrieve_vars(Bin) ->
%     {ok, Tokens, _} = erl_scan:string(erlang:binary_to_list(Bin)),
%     MaybeAcc =
%         fun(Var, VAcc) ->
%             case lists:member(Var, VAcc) of
%                 true -> VAcc;
%                 false -> [Var | VAcc]
%             end
%         end,
%     {_, Vars} =
%         lists:foldl(
%             fun
%                 ({var, _, Var} = Token, {[{':=', _} | _] = TAcc, VAcc}) ->
%                     {[Token | TAcc], MaybeAcc(Var, VAcc)};
%                 ({var, _, _} = Token, {[{'=>', _} | _] = TAcc, VAcc}) ->
%                     {[Token | TAcc], VAcc};
%                 ({var, _, Var} = Token, {TAcc, VAcc}) ->
%                     {[Token | TAcc], MaybeAcc(Var, VAcc)};
%                 (Token, {TAcc, VAcc}) ->
%                     {[Token | TAcc], VAcc}
%             end,
%             {[], []},
%             Tokens
%         ),
%     Vars.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

handle_expr_test() ->
    [
        {
            "Should return expr token",
            ?assertEqual(
                #state{tokens = [{expr, <<"Foo">>}]},
                handle_expr({1, 1}, {"<%=", ".%>"}, <<"Foo">>, #state{})
            )
        },
        {
            "Should return start_expr token",
            ?assertEqual(
                #state{tokens = [{start_expr, <<"Foo">>}]},
                handle_expr({1, 1}, {"<%=", "%>"}, <<"Foo">>, #state{})
            )
        },
        {
            "Should return mid_expr token",
            ?assertEqual(
                #state{tokens = [{mid_expr, <<"Foo">>}]},
                handle_expr({1, 1}, {"<%", "%>"}, <<"Foo">>, #state{})
            )
        },
        {
            "Should return end_expr token",
            ?assertEqual(
                #state{tokens = [{end_expr, <<"Foo">>}]},
                handle_expr({1, 1}, {"<%", ".%>"}, <<"Foo">>, #state{})
            )
        },
        {
            "Should return debug token",
            ?assertEqual(
                #state{tokens = [{debug, <<"io:format(Foo)">>}]},
                handle_expr({1, 1}, {"<%:", ":%>"}, <<"io:format(Foo)">>, #state{})
            )
        },
        {
            "Should ignore comment",
            ?assertEqual(
                #state{tokens = [{comment, <<"Foo">>}]},
                handle_expr({1, 1}, {"<%%", "%%>"}, <<"Foo">>, #state{})
            )
        },
        {
            "Should raise unknown marker error",
            ?assertError(
                unknown_marker,
                handle_expr({1, 1}, {"<%.", ".%>"}, <<"Foo">>, #state{})
            )
        }
    ].

handle_text_test() ->
    [
        {
            "Should return text token",
            ?assertEqual(
                #state{tokens = [{text, <<"Foo">>}]},
                handle_text({1, 1}, <<"Foo">>, #state{})
            )
        }
    ].

handle_body_test() ->
    Bin = <<
        "<h1>Title</h1>"
        "<%: io:format(\"Print but not render me!~n\") :%>"
        "<%= case 1 of %>"
        "<% 2 -> %><p>Foo</p>"
        "<% ; Bar -> %>"
            "<p>"
                "<%= case hello =:= world of %>"
                "<% true -> %>"
                    "<%= hello .%>"
                "<% ; false -> %>"
                    "<p>"
                        "<%% This is a comment %%>"
                        "<%: ignore_me :%>"
                        "<%= case car =:= bus of %>"
                        "<% true -> %>"
                            "Car"
                        "<% ; false -> %>"
                            "<%= bus .%>"
                        "<% end .%>"
                    "</p>"
                "<% end .%>"
            "</p>"
        "<% end .%>"
        "<footer>Footer</footer>"
    >>,
    Expected = [
        {text,<<"<h1>Title</h1>">>},
        {debug,<<"io:format(\"Print but not render me!~n\")">>},
        {start_expr,<<"case 1 of">>},
        {mid_expr,<<"2 ->">>},
        {text,<<"<p>Foo</p>">>},
        {mid_expr,<<"; Bar ->">>},
        {text,<<"<p>">>},
        {start_expr,<<"case hello =:= world of">>},
        {mid_expr,<<"true ->">>},
        {expr,<<"hello">>},
        {mid_expr,<<"; false ->">>},
        {text,<<"<p>">>},
        {comment,<<"This is a comment">>},
        {debug,<<"ignore_me">>},
        {start_expr,<<"case car =:= bus of">>},
        {mid_expr,<<"true ->">>},
        {text,<<"Car">>},
        {mid_expr,<<"; false ->">>},
        {expr,<<"bus">>},
        {end_expr,<<"end">>},
        {text,<<"</p>">>},
        {end_expr,<<"end">>},
        {text,<<"</p>">>},
        {end_expr,<<"end">>},
        {text,<<"<footer>Footer</footer>">>}
    ],
    Tokens = eel_tokenizer:tokenize(Bin, ?MODULE, []),
    ?assertEqual(Expected, Tokens).

-endif.
