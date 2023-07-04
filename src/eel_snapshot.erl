-module(eel_snapshot).

-export([ new/3
        , new/4
        , new/5
        , new/7
        , get_static/1
        , get_dynamic/1
        , get_ast/1
        , get_bindings/1
        , get_vars/1
        , get_changes/1
        , get_state/1
        , set_state/2
        ]).

-export_type([ snapshot/0
             , bindings/0
             , static/0
             , dynamic/0
             , ast/0
             , vars/0
             , changes/0
             , state/0
             ]).

-record(snapshot, { static   :: static()
                  , dynamic  :: dynamic()
                  , ast      :: ast()
                  , bindings :: bindings()
                  , vars     :: vars()
                  , changes  :: changes()
                  , state    :: state()
                  }).

%% Types
-type snapshot() :: #snapshot{}.
-type bindings() :: eel_engine:bindings().
-type static()   :: eel_engine:static().
-type dynamic()  :: undefined | [binary()].
-type ast()      :: eel_engine:ast().
-type vars()     :: [atom()].
-type changes()  :: [{non_neg_integer(), binary()}].
-type state()    :: eel_engine:state().

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc new/3.
%% @end
%% -----------------------------------------------------------------------------
-spec new(Static, AST, State) -> Result
    when Static :: static()
       , AST    :: ast()
       , State  :: state()
       , Result :: snapshot()
       .

new(Static, AST, State) ->
    new(Static, undefined, AST, State).

%% -----------------------------------------------------------------------------
%% @doc new/4.
%% @end
%% -----------------------------------------------------------------------------
-spec new(Static, Dynamic, AST, State) -> Result
    when Static  :: static()
       , Dynamic :: dynamic()
       , AST     :: ast()
       , State   :: state()
       , Result  :: snapshot()
       .

new(Static, Dynamic, AST, State) ->
    new(Static, Dynamic, AST, #{}, State).

%% -----------------------------------------------------------------------------
%% @doc new/5.
%% @end
%% -----------------------------------------------------------------------------
-spec new(Static, Dynamic, AST, Bindings, State) -> Result
    when Static   :: static()
       , Dynamic  :: dynamic()
       , AST      :: ast()
       , Bindings :: bindings()
       , State    :: state()
       , Result   :: snapshot()
       .

new(Static, Dynamic, AST, Bindings, State) ->
    Vars = eel_compiler:ast_vars(AST),
    new(Static, Dynamic, AST, Bindings, Vars, [], State).

%% -----------------------------------------------------------------------------
%% @doc new/7.
%% @end
%% -----------------------------------------------------------------------------
-spec new(Static, Dynamic, AST, Bindings, Vars, Changes, State) -> Result
    when Static   :: static()
       , Dynamic  :: dynamic()
       , AST      :: ast()
       , Bindings :: bindings()
       , Vars     :: [atom()]
       , Changes  :: changes()
       , Result   :: snapshot()
       , State    :: state()
       .

new(Static, Dynamic, AST, Bindings, Vars, Changes, State) ->
    #snapshot{ static   = Static
             , dynamic  = Dynamic
             , ast      = AST
             , bindings = Bindings
             , vars     = Vars
             , changes  = Changes
             , state    = State
             }.

get_static(#snapshot{static = Static}) ->
    Static.

get_dynamic(#snapshot{dynamic = Dynamic}) ->
    Dynamic.

get_ast(#snapshot{ast = AST}) ->
    AST.

get_bindings(#snapshot{bindings = Bindings}) ->
    Bindings.

get_vars(#snapshot{vars = Vars}) ->
    Vars.

get_changes(#snapshot{changes = Changes}) ->
    Changes.

get_state(#snapshot{state = State}) ->
    State.

set_state(State, #snapshot{} = Snapshot) ->
    Snapshot#snapshot{state = State}.
