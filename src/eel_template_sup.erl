%%%-----------------------------------------------------------------------------
%%% @doc EEl template supervisor.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_template_sup).

-behaviour(supervisor).

%% API functions
-export([
    start_link/0,
    start_child/2,
    terminate_child/1
]).

%% supervisor callbacks
-export([init/1]).

%% Defines
-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc start_link.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> supervisor:startlink_ret().

start_link() ->
    InitArgs = [],
    supervisor:start_link({local, ?SERVER}, ?MODULE, InitArgs).

%%------------------------------------------------------------------------------
%% @doc start_child.
%% @end
%%------------------------------------------------------------------------------
-spec start_child(eel_output:module_def(), eel_output:filename() | module()) ->
    supervisor:startchild_ret().

start_child(ModuleDef, FileOrModName) ->
    supervisor:start_child(?MODULE, [ModuleDef, FileOrModName]).

%%------------------------------------------------------------------------------
%% @doc terminate_child.
%% @end
%%------------------------------------------------------------------------------
-spec terminate_child(pid()) -> ok | {error, not_found | simple_one_for_one}.

terminate_child(Template) ->
    supervisor:terminate_child(?MODULE, Template).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc init.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init(_InitArgs) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => eel_template,
            start => {eel_template, start_link, []}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here yet!
