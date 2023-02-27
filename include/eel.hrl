-define(DEFAULT_ENGINE, eel_smart_engine).
-define(DEFAULT_ENGINE_OPTS, #{
    % capitalize bindings keys
    % e.g. #{foo_bar => baz} -> #{'FooBar' => baz}
    % note: eval expects capitalized atoms
    capitalize => false
}).
