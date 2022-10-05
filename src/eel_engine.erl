-module(eel_engine).

-type line() :: pos_integer().
-type column() :: pos_integer().
-type marker() :: binary().
-type expression() :: binary().

-type cursor() :: {line(), column()}.
-type markers() :: {Start :: marker(), End :: marker()}.
-type expressions() :: {Outside :: expression(), Inside :: expression()}.

-callback handle_expr({cursor(), markers(), expressions()}, list()) -> list().
