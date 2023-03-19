-compile({parse_transform, eel_transform}).

% Binary

-define(eel_fun_from_bin(FunName, Bin, Opts, FunOpts),
    -eel_fun({FunName, {bin, Bin, Opts}, FunOpts})
).

-define(eel_fun_from_bin(FunName, Bin, Opts),
    ?eel_fun_from_bin(FunName, Bin, Opts, #{eval => true})
).

-define(eel_fun_from_bin(FunName, Bin),
    ?eel_fun_from_bin(FunName, Bin, #{})
).

% File

-define(eel_fun_from_file(FunName, Filename, Opts, FunOpts),
    -eel_fun({FunName, {file, Filename, Opts}, FunOpts})
).

-define(eel_fun_from_file(FunName, Filename, Opts),
    ?eel_fun_from_file(FunName, Filename, Opts, #{eval => true})
).

-define(eel_fun_from_file(FunName, Filename),
    ?eel_fun_from_file(FunName, Filename, #{})
).
