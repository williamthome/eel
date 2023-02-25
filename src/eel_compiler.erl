%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl compiler module.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_compiler).

%% API functions
-export([compile/1, compile/2,
         expr_to_ast/1,
         normalize_expr/1,
         merge_sd/1, merge_sd/2]).

%% Includes
-include("eel.hrl").

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc compile/1.
%% @end
%% -----------------------------------------------------------------------------
-spec compile(eel_tokenizer:tokens()) -> list().

compile(Tokens) ->
    compile(Tokens, ?DEFAULT_ENGINE_OPTS).

%% -----------------------------------------------------------------------------
%% @doc compile/2.
%% @end
%% -----------------------------------------------------------------------------
-spec compile(eel_tokenizer:tokens(), map()) -> list().

compile({Static, Dynamic}, Opts) when is_list(Static), is_list(Dynamic) ->
    Eng = maps:get(engine, Opts, ?DEFAULT_ENGINE),
    State = Eng:init(Opts),
    DAST = do_compile(Dynamic, Eng, State),
    SAST = lists:map(fun(S) -> expr_to_ast(["<<\"", S, "\">>"]) end, Static),
    merge_sd(SAST, DAST).

%% -----------------------------------------------------------------------------
%% @doc expr_to_ast/1.
%% @end
%% -----------------------------------------------------------------------------
-spec expr_to_ast(binary() | string()) -> eel_engine:ast().

expr_to_ast(Expr) ->
    {ok, Tokens, _} = erl_scan:string(normalize_expr(Expr)),
    {ok, AST} = erl_parse:parse_exprs(Tokens),
    AST.

%% -----------------------------------------------------------------------------
%% @doc normalize_expr/1.
%% @end
%% -----------------------------------------------------------------------------
-spec normalize_expr(binary() | string()) -> string().

normalize_expr(Expr) ->
    erlang:binary_to_list(erlang:iolist_to_binary([Expr, "."])).

%% -----------------------------------------------------------------------------
%% @doc merge_sd/1.
%% @end
%% -----------------------------------------------------------------------------
-spec merge_sd(eel_tokenizer:tokens()) -> list().

merge_sd({Static, Dynamic}) ->
    merge_sd(Static, Dynamic).

%% -----------------------------------------------------------------------------
%% @doc merge_sd/2.
%% @end
%% -----------------------------------------------------------------------------
-spec merge_sd([binary()], list()) -> list().

merge_sd(Static, Dynamic) ->
    do_merge_sd(Static, Dynamic, []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_compile([D | Dynamic], Eng, State) ->
    NewState = Eng:handle_compile(D, State),
    do_compile(Dynamic, Eng, NewState);
do_compile([], Eng, State) ->
    Eng:handle_ast(State).

do_merge_sd([S | Static], [D | Dynamic], Acc) ->
    do_merge_sd(Static, Dynamic, [D, S | Acc]);
do_merge_sd([S | Static], [], Acc) ->
    do_merge_sd(Static, [], [S | Acc]);
do_merge_sd([], [], Acc) ->
    lists:reverse(Acc);
do_merge_sd([], Dynamic, Acc) ->
    lists:reverse(Dynamic, Acc).
