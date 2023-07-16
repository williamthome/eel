-module(eel_snapshot).

-export([ new/4
        , new/5
        , new/6
        , new/8
        , get_root/1
        , get_static/1
        , set_static/2
        , get_dynamic/1
        , set_dynamic/2
        , get_ast/1
        , set_ast/2
        , get_bindings/1
        , set_bindings/2
        , get_vars/1
        , set_vars/2
        , get_changes/1
        , set_changes/2
        , get_state/1
        , set_state/2
        ]).

-export_type([ root/0
             , snapshot/0
             , bindings/0
             , static/0
             , dynamic/0
             , ast/0
             , vars/0
             , changes/0
             , state/0
             ]).

-record(snapshot, { root     :: root()
                  , static   :: static()
                  , dynamic  :: dynamic()
                  , ast      :: ast()
                  , bindings :: bindings()
                  , vars     :: vars()
                  , changes  :: changes()
                  , state    :: state()
                  }).

%% Types
-type root()     :: binary | {file, file:filename_all()}.
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
%% @doc new/4.
%% @end
%% -----------------------------------------------------------------------------
-spec new(Root, Static, AST, State) -> Result
    when Root   :: root()
       , Static :: static()
       , AST    :: ast()
       , State  :: state()
       , Result :: snapshot()
       .

new(Root, Static, AST, State) ->
    new(Root, Static, undefined, AST, State).

%% -----------------------------------------------------------------------------
%% @doc new/5.
%% @end
%% -----------------------------------------------------------------------------
-spec new(Root, Static, Dynamic, AST, State) -> Result
    when Root    :: root()
       , Static  :: static()
       , Dynamic :: dynamic()
       , AST     :: ast()
       , State   :: state()
       , Result  :: snapshot()
       .

new(Root, Static, Dynamic, AST, State) ->
    new(Root, Static, Dynamic, AST, #{}, State).

%% -----------------------------------------------------------------------------
%% @doc new/6.
%% @end
%% -----------------------------------------------------------------------------
-spec new(Root, Static, Dynamic, AST, Bindings, State) -> Result
    when Root     :: root()
       , Static   :: static()
       , Dynamic  :: dynamic()
       , AST      :: ast()
       , Bindings :: bindings()
       , State    :: state()
       , Result   :: snapshot()
       .

new(Root, Static, Dynamic, AST, Bindings, State) ->
    Vars = eel_compiler:ast_vars(AST),
    new(Root, Static, Dynamic, AST, Bindings, Vars, [], State).

%% -----------------------------------------------------------------------------
%% @doc new/8.
%% @end
%% -----------------------------------------------------------------------------
-spec new(Root, Static, Dynamic, AST, Bindings, Vars, Changes, State) -> Result
    when Root     :: root()
       , Static   :: static()
       , Dynamic  :: dynamic()
       , AST      :: ast()
       , Bindings :: bindings()
       , Vars     :: [atom()]
       , Changes  :: changes()
       , Result   :: snapshot()
       , State    :: state()
       .

new(Root, Static, Dynamic, AST, Bindings, Vars, Changes, State) ->
    #snapshot{ root     = Root
             , static   = Static
             , dynamic  = Dynamic
             , ast      = AST
             , bindings = Bindings
             , vars     = Vars
             , changes  = Changes
             , state    = State
             }.

get_root(#snapshot{root = Root}) ->
    Root.

get_static(#snapshot{static = Static}) ->
    Static.

set_static(Static, #snapshot{} = Snapshot) ->
    Snapshot#snapshot{static = Static}.

get_dynamic(#snapshot{dynamic = Dynamic}) ->
    Dynamic.

set_dynamic(Dynamic, #snapshot{} = Snapshot) ->
    Snapshot#snapshot{dynamic = Dynamic}.

get_ast(#snapshot{ast = AST}) ->
    AST.

set_ast(AST, #snapshot{} = Snapshot) ->
    Snapshot#snapshot{ast = AST}.

get_bindings(#snapshot{bindings = Bindings}) ->
    Bindings.

set_bindings(Bindings, #snapshot{} = Snapshot) ->
    Snapshot#snapshot{bindings = Bindings}.

get_vars(#snapshot{vars = Vars}) ->
    Vars.

set_vars(Vars, #snapshot{} = Snapshot) ->
    Snapshot#snapshot{vars = Vars}.

get_changes(#snapshot{changes = Changes}) ->
    Changes.

set_changes(Changes, #snapshot{} = Snapshot) ->
    Snapshot#snapshot{changes = Changes}.

get_state(#snapshot{state = State}) ->
    State.

set_state(State, #snapshot{} = Snapshot) ->
    Snapshot#snapshot{state = State}.
