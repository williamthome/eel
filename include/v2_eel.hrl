-record(marker, {
    id :: atom(),
    start :: binary(),
    'end' :: binary(),
    tree_behavior :: open | push | close | ignore
}).
