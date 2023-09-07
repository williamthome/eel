-record(marker, {
    id    :: marker_id(),
    start :: marker_symbol(),
    final :: marker_symbol(),
    tree_behavior :: marker_tree_behavior()
}).

-type marker_id() :: term().
-type marker_symbol() :: binary().
-type marker_tree_behavior() :: open | push | close | ignore.