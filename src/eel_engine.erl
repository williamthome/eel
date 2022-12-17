-module(eel_engine).

-type line()        :: pos_integer().
-type column()      :: pos_integer().
-type expression()  :: binary().
-type state()       :: term().
-type tokens()      :: term().
-type ast()         :: list().
-type position()    :: {line(), column()}.
-type marker()      :: {Start :: nonempty_string(), End :: nonempty_string()}.
-type expressions() :: {Outside :: expression(), Inside :: expression()}.
-type options()     :: term().

-callback init(options()) -> state().

-callback markers() -> [marker()].

-callback handle_expr(position(), marker(), expressions(), state()) -> state().

-callback handle_text(position(), binary(), state()) -> state().

-callback handle_body(state()) -> ast().

-callback compile(tokens(), options()) -> ast().
