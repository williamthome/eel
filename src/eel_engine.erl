-module(eel_engine).

-type line() :: pos_integer().
-type column() :: pos_integer().
-type marker() :: binary().
-type expression() :: binary().
-type state() :: term().
-type ast() :: list().

-type cursor() :: {line(), column()}.
-type markers() :: {Start :: marker(), End :: marker()}.
-type expressions() :: {Outside :: expression(), Inside :: expression()}.

-callback init(term()) ->
    {ok, {{nonempty_string(), nonempty_string()}, state()}} | {error, term()}.

-callback handle_expr({cursor(), markers(), expressions()}, state()) ->
    {ok, state()} | {error, term()}.

-callback handle_text({cursor(), binary()}, state()) ->
    {ok, state()} | {error, term()}.

-callback handle_body(state()) ->
    {ok, ast()} | {error, term()}.
