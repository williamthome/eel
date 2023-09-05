-record(marker, {
    id :: atom(),
    start :: binary(),
    'end' :: binary(),
    kind = singleton :: singleton | multiplex | snapshot
}).
