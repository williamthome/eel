-module(eel_engine).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type line()        :: pos_integer().
-type column()      :: pos_integer().
-type expression()  :: binary().
-type state()       :: term().
-type token()       :: term().
-type static()      :: [binary()].
-type dynamic()     :: [token()].
-type ast()         :: list().
-type position()    :: {line(), column()}.
-type marker_id()   :: atom().
-type marker()      :: nonempty_string().
-type marker_def()  :: {marker_id(), {Start :: marker(), End :: marker()}}.
-type expressions() :: {Outside :: expression(), Inside :: expression()}.
-type options()     :: map().

-export_type([line/0, column/0, expression/0, state/0, token/0, static/0,
              dynamic/0, ast/0, position/0, marker_id/0, marker/0, marker_def/0,
              expressions/0, options/0]).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-callback markers() -> [marker_def()].
-callback init(options()) -> state().

%% tokenize
-callback handle_expr(position(), marker_id(), expressions(), state()) -> state().
-callback handle_text(position(), binary(), state()) -> state().
-callback handle_body(state()) -> {static(), dynamic()}.

%% compile
-callback handle_compile(token(), state()) -> state().
-callback handle_ast(state()) -> ast().
