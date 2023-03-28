-compile({parse_transform, eel_transform}).

% Binary

-define(eel_bin(Bin, Opts), {eel, {fun_from_binary, {Bindings, Bin, Opts}}}).

-define(eel_bin(Bin), ?eel_bin(Bin, #{})).

-define(eel_bin_render(Bin, Opts), ?eel_bin(Bin, Opts#{result => render})).

-define(eel_bin_render(Bin), ?eel_bin_render(Bin, #{})).

-define(eel_bin_eval(Bin, Opts), ?eel_bin(Bin, Opts#{result => eval})).

-define(eel_bin_eval(Bin), ?eel_bin_eval(Bin, #{})).

% File

-define(eel_file(Filename, Opts), {eel, {fun_from_file, {Bindings, Filename, Opts}}}).

-define(eel_file(Filename), ?eel_file(Filename, #{})).

-define(eel_file_render(Filename, Opts), ?eel_file(Filename, Opts#{result => render})).

-define(eel_file_render(Filename), ?eel_file_render(Filename, #{})).

-define(eel_file_eval(Filename, Opts), ?eel_file(Filename, Opts#{result => eval})).

-define(eel_file_eval(Filename), ?eel_file_eval(Filename, #{})).
