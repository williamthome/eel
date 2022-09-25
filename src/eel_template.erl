%%%-----------------------------------------------------------------------------
%%% @doc EEl template gen_server.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @end
%%%-----------------------------------------------------------------------------
-module(eel_template).

-behaviour(gen_server).

%% API functions
-export([
    start_link/2,
    filename/1,
    static/1,
    ast/1,
    render/1,
    render/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(state, {
    module :: module(),
    memo = #{} :: eel_render:memo()
}).

%% Types
-type state() :: #state{}.
-type from() :: {pid(), gen_server:reply_tag()}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc start_link.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(eel_output:module_def(), eel_output:filename() | module()) ->
    gen:start_ret().

start_link(ModuleDef, FileOrModName) ->
    {ok, Module} = eel_output:to_module(ModuleDef, FileOrModName),
    gen_server:start_link(?MODULE, [Module], []).

filename(Template) ->
    gen_server:call(Template, {filename, []}).

static(Template) ->
    gen_server:call(Template, {static, []}).

ast(Template) ->
    gen_server:call(Template, {ast, []}).

render(Template) ->
    render(Template, #{}).

render(Template, Bindings) ->
    gen_server:call(Template, {render, Bindings}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc init.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {ok, #state{}}.

init([Module]) ->
    State = #state{
        module = Module
    },
    {ok, State}.

%%------------------------------------------------------------------------------
%% @doc handle_call.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(
    {render, eel_render:bindings()} | {atom(), list()},
    from(),
    state()
) ->
    {reply, term(), state()}.

handle_call({render, Bindings}, _From, #state{module = Mod, memo = Memo} = State0) ->
    {Rendered, NewMemo, Indexes} = erlang:apply(Mod, render, [Memo, Bindings]),
    State = State0#state{
        memo = NewMemo
    },
    Reply = {Rendered, Indexes},
    {reply, Reply, State};
handle_call({Fun, Args}, _From, #state{module = Mod} = State) ->
    Reply = erlang:apply(Mod, Fun, Args),
    {reply, Reply, State}.

%%------------------------------------------------------------------------------
%% @doc handle_cast.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here yet!
