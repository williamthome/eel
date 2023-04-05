-module(eel_test_support).

-export([temp_dir/0, get_template/1]).

-define(ENV_GITHUB_TMP_DIR, "RUNNER_TEMP").

temp_dir() ->
    os:getenv(?ENV_GITHUB_TMP_DIR, "/tmp").

get_template(Filename) ->
    {ok, CWD} = file:get_cwd(),
    filename:join([CWD, "test", "support", "templates", Filename]).

