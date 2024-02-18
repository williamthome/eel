-record(marker, {
    id             :: marker_id(),
    start          :: marker_symbol(),
    final          :: marker_symbol(),
    start_groups   :: [marker_regex_group()],
    final_groups   :: [marker_regex_group()],
    tree_behaviors :: [marker_tree_behavior()],
    compile_as     :: marker_compile_as()
}).

-record(text_token, { text     :: binary()
                    , metadata :: term()
                    }).

-record(expr_token, { expr     :: binary()
                    , engine   :: engine()
                    , marker   :: #marker{}
                    , vars     :: [atom()]
                    , metadata :: term()
                    }).

-record(engine_metadata, { module  :: engine()
                         , markers :: [#marker{}]
                         , state   :: term()
                         }).

-record(master_vertex, { ast :: ast() }).

-record(slave_vertex, { token :: token() }).

-record(tree_state, { master_vertices :: [#master_vertex{}] }).

-type marker_id()            :: term().
-type marker_symbol()        :: binary().
-type marker_regex_group()   :: binary().
-type marker_tree_behavior() :: add_vertex
                              | push_token
                              | fetch_vertex_parent
                              | ignore_token
                              | fun( (token(), eel_tree:label(), eel_tree:tree()) ->
                                     {eel_tree:label(), eel_tree:tree()} )
                              .
-type marker_compile_as()    :: expr | expr_start | undefined.

-type engine()     :: module().
-type text_token() :: #text_token{}.
-type expr_token() :: #expr_token{}.
-type token()      :: text_token() | expr_token().
-type ast()        :: erl_syntax:syntaxTree().
