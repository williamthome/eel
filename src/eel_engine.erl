-module(eel_engine).

-type line() :: pos_integer().
-type column() :: pos_integer().
-type marker() :: binary().
-type expression() :: binary().

-type cursor() :: {line(), column()}.
-type markers() :: {Start :: marker(), End :: marker()}.
-type expressions() :: {Outside :: expression(), Inside :: expression()}.

-callback init(term()) -> term().

-callback handle_expr({cursor(), markers(), expressions()}, list(), term()) ->
    {ok, {list(), term()}} | {error, term()}.

-callback handle_text({cursor(), binary()}, list(), term()) ->
    {ok, {list(), term()}} | {error, term()}.

-callback handle_body(list(), term()) ->
    {ok, list()} | {error, term()}.
