# EEl

Much like Elixir has EEx, Erlang has EEl, or Embedded Erlang. With EEl we can embed and evaluate Erlang inside strings.

## API

The Erlang code it's written between section punctuations called `markers`.
In a nutshell, an IO data or a file can be evaluated

```erlang
1> eel:eval(<<"Hello, <%= Name .%>!">>, #{'Name' => <<"World">>}).
["Hello, ",<<"World">>,"!"]
```

a file compiled to a module

```erlang
1> eel:to_module(<<"Hello, <%= Name .%>!">>, foo).
{ok,foo}
2> foo:eval(#{'Name' => <<"World">>}).
["Hello, ",<<"World">>,"!"]
```

or a set of functions used in a module, for example, given this module

```erlang
-module(foo).

-export([render/1, render/2]).

% Including the header will transform the `eel:compile` to AST
% by evaluating it in te compile time, boosting the performance.
-include("eel.hrl").

render(Bindings) ->
    {ok, Snapshot} = eel:compile(<<
        "<html>"
        "<head>"
            "<title><%= Title .%></title>"
        "</head>"
        "<body>"
            "Hello, <%= Name .%>!"
        "</body>"
        "</html>"
    >>),
    render(Bindings, Snapshot).

render(Bindings, Snapshot) ->
    {ok, RenderSnapshot} = eel_renderer:render(Bindings, Snapshot),
    {eel_evaluator:eval(RenderSnapshot), RenderSnapshot}.
```

now run

```shell
rebar3 shell
```

and type this in the Erlang shell

```erlang
1> {_, Snapshot} = foo:render(#{'Title' => <<"Hey!">>, 'Name' => <<"World">>}).
{["<html><head><title>",<<"Hey!">>,
  "</title></head><body>Hello, ",<<"World">>,
  "!</body></html>"],
 #{ast =>
       [{2,
         {{1,20},
          [{call,1,
               {remote,1,{atom,1,eel_converter},{atom,1,to_string}},
               [{'fun',1,
                    {clauses,[{clause,1,[],[],[{var,1,'Title'}]}]}}]}]}},
        {4,
         {{1,61},
          [{call,1,
               {remote,1,{atom,1,eel_converter},{atom,1,to_string}},
               [{'fun',1,
                    {clauses,[{clause,1,[],[],[{var,1,...}]}]}}]}]}}],
   bindings => #{'Name' => <<"World">>,'Title' => <<"Hey!">>},
   changes => [{2,<<"Hey!">>},{4,<<"World">>}],
   dynamic =>
       [{2,{{1,20},<<"Hey!">>}},{4,{{1,61},<<"World">>}}],
   static =>
       [{1,{{1,1},"<html><head><title>"}},
        {3,{{1,33},"</title></head><body>Hello, "}},
        {5,{{1,73},"!</body></html>"}}],
   vars => [{2,['Title']},{4,['Name']}]}}
2> {Bin, _} = foo:render(#{'Name' => <<"Erlang">>}, Snapshot).
{["<html><head><title>",<<"Hey!">>,
  "</title></head><body>Hello, ",<<"Erlang">>,
  "!</body></html>"],
 #{ast =>
       [{2,
         {{1,20},
          [{call,1,
               {remote,1,{atom,1,eel_converter},{atom,1,to_string}},
               [{'fun',1,
                    {clauses,[{clause,1,[],[],[{var,1,'Title'}]}]}}]}]}},
        {4,
         {{1,61},
          [{call,1,
               {remote,1,{atom,1,eel_converter},{atom,1,to_string}},
               [{'fun',1,
                    {clauses,[{clause,1,[],[],[{var,1,...}]}]}}]}]}}],
   bindings => #{'Name' => <<"Erlang">>,'Title' => <<"Hey!">>},
   changes => [{4,<<"Erlang">>}],
   dynamic =>
       [{2,{{1,20},<<"Hey!">>}},{4,{{1,61},<<"Erlang">>}}],
   static =>
       [{1,{{1,1},"<html><head><title>"}},
        {3,{{1,33},"</title></head><body>Hello, "}},
        {5,{{1,73},"!</body></html>"}}],
   vars => [{2,['Title']},{4,['Name']}]}}
```

Looking at the pattern matched results, the first tuple element contains the evaluated value
```erlang
["<html><head><title>",<<"Hey!">>,
 "</title></head><body>Hello, ",<<"World">>,
 "!</body></html>"],
```
and the second a metadata called `snapshot` (see next)
```erlang
#{ast =>
       [{2,
         {{1,20},
          [{call,1,
               {remote,1,{atom,1,eel_converter},{atom,1,to_string}},
               [{'fun',1,
                    {clauses,[{clause,1,[],[],[{var,1,'Title'}]}]}}]}]}},
        {4,
         {{1,61},
          [{call,1,
               {remote,1,{atom,1,eel_converter},{atom,1,to_string}},
               [{'fun',1,
                    {clauses,[{clause,1,[],[],[{var,1,...}]}]}}]}]}}],
   bindings => #{'Name' => <<"World">>,'Title' => <<"Hey!">>},
   changes => [{2,<<"Hey!">>},{4,<<"World">>}],
   dynamic =>
       [{2,{{1,20},<<"Hey!">>}},{4,{{1,61},<<"World">>}}],
   static =>
       [{1,{{1,1},"<html><head><title>"}},
        {3,{{1,33},"</title></head><body>Hello, "}},
        {5,{{1,73},"!</body></html>"}}],
   vars => [{2,['Title']},{4,['Name']}]}
```

The line `1` will evaluate the bindings `Title` and `Name`, but the line `2`
will only eval the `Name` variable, because it uses the snapshot of the previous
render. It only eval the changes and does not need to compile the binary again, unless the expression contains the global `Bindings` variable (see below),
because the `snapshot` includes the required information.

The var `Bindings` is a reserved one and can be used to get values in a conditional way, checking if the variable exists in the template, e.g.:

```
<%= maps:get('Foo', Bindings, bar) .%>
```

The `Bindings` should contains the unbound/required variables of the template. The syntax it's a map with keys as atoms starting with upper case, e.g:

```erlang
#{'Foo' => <<"foo">>, 'FooBar' => bar}
```

or the same in lower case when passing the option #{snake_case => true} to the compile function. Passing the snake_case option the bindings above you must write the keys as

```erlang
#{foo => <<"foo">>, foo_bar => bar}
```

Including `Bindings` to the expression makes it to be always evaluated by the render function.

## Engine

The default engine is the `eel_smart_engine`.\
You can implement your own engine using the behavior `eel_engine`.

### eel_smart_engine

#### Markers

The `eel_smart_engine` markers are:
- `<%=` starts an expression;
- `.%>` indicates that the expression ends;
- `%>`  indicates that the expression continues;
- `<%`  continues the last expression if it ends with `%>`;
- `<%%` starts a comment;
- `%%>` ends a comment.

## Template

The template should have the `.eel` extension and a structure like this:

![Template](images/template.png)

## Highlight

If you use VSCode, you can get highlighting by installing the [Embedded Erlang (EEl)](https://github.com/williamthome/vscode_eel) extension.

## Next steps

- Explain how this lib works
- Better explanation about the API
- Improve the code
- Functions documentations
- Functions specs
- Test everything

## Sponsors

If you like this tool, please consider [sponsoring me](https://github.com/sponsors/williamthome).\
I'm thankful for your never-ending support :heart:

I also accept coffees :coffee:

[!["Buy Me A Coffee"](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/williamthome)

## Contributing

### Issues

Feels free to [submit an issue on Github](https://github.com/williamthome/eel/issues/new).

### Installation

```shell
# Clone this repo
git clone git@github.com:williamthome/eel.git

# Navigate to the project root
cd eel

# Compile (ensure you have rebar3 installed)
rebar3 compile
```
