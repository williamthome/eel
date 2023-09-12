-record(marker, {
    id    :: marker_id(),
    start :: marker_symbol(),
    final :: marker_symbol(),
    tree_behavior :: marker_tree_behavior()
}).

-record(text_token, { text         :: binary()
                    , handled_text :: binary()
                    , ast          :: ast()
                    }).

-record(expr_token, { expr         :: binary()
                    , handled_expr :: binary()
                    , engine       :: engine()
                    , marker       :: #marker{} | none
                    , vars         :: [atom()]
                    , ast          :: ast()
                    }).

-type marker_id()            :: term().
-type marker_symbol()        :: binary().
-type marker_tree_behavior() :: add_vertex
                              | push_token
                              | fetch_vertex_parent
                              | ignore_token
                            %   TODO: Custom fun
                            %   | fun((...) -> term())
                              .
-type engine()     :: module().
-type text_token() :: #text_token{}.
-type expr_token() :: #expr_token{}.
-type token()      :: text_token() | expr_token().
-type ast()        :: erl_syntax:syntaxTree().
