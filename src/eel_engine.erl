%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc EEl engine behaviour.
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_engine).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type index()         :: pos_integer().
-type line()          :: pos_integer().
-type column()        :: pos_integer().
-type position()      :: {line(), column()}.
-type expression()    :: binary().
-type state()         :: term().
-type token()         :: {index(), {position(), expression()}}.
-type static()        :: [token()].
-type dynamic()       :: [token()].
-type ast()           :: erl_syntax:syntaxTree().
-type marker_id()     :: term().
-type marker_symbol() :: binary().
-type marker_size()   :: non_neg_integer().
-type marker()        :: {marker_id(), { Start :: marker_symbol()
                                       , End   :: marker_symbol() }}.
-type expressions()   :: {Outer :: expression(), Inner :: expression()}.
-type options()       :: map().

-export_type([ index/0
             , line/0
             , column/0
             , expression/0
             , state/0
             , token/0
             , static/0
             , dynamic/0
             , ast/0
             , position/0
             , marker_id/0
             , marker_symbol/0
             , marker/0
             , expressions/0
             , options/0
             ]).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-callback init(options()) -> {ok, state()} | {error, term()}.

%% tokenize
-callback handle_expr_start( binary()
                           , state() ) -> {ok, {StartMarker :: marker_symbol(), marker_size(), Rest :: binary(), state()}} | {error, term()}.

-callback handle_expr_end( StartMarker :: marker_symbol()
                         , binary()
                         , state() ) -> {ok, state()} | {error, term()}.

-callback handle_text( iodata()
                     , position()
                     , state() ) -> {ok, {iodata(), position(), state()}} | {error, term()}.

-callback handle_body( [token()]
                     , state() ) -> {ok, {static(), dynamic()}} | {error, term()}.

%% compile
-callback handle_compile( [token()]
                        , state()) -> {ok, state()} | {error, term()}.

-callback handle_ast( ast(), state() ) -> {ok, ast()} | {error, term()}.
