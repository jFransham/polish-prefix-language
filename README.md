# Polish prefix notation-based language

W/ hints of Lisp, to taste.

There are just two kinds of function: unary and binary. To receive more
arguments one must take a list. Unary functions are applied with no whitespace,
binary functions are applied with whitespace. Macros are denoted by a `$` and
simply consume expressions until a matching closing brace or a `;`. Functions,
are an optional string of alphanumeric characters (or `_`) followed by a string
of punctuation characters, but other expressions (such as variables not ending
in punctuation) can be used as functions by appending a `:` (no whitespace).

Example:

```
print:"Hello, world!";

$def do_thing: a b
    + a b;

$def % fn a (
    print:fn;
    fn:a
);

% (-) do_thing: 1 2
```

Prints

```
Hello, world!
($fn a b {{builtin}})
```

and outputs -3

Features implemented:
* Opt-in memoisation: currently implemented in the runtime, but memoised
functions can already be created without the runtime support - it's just super
unergonomic without macros. See below.
* Pattern-matching: includes wildcards (`_`) and deconstructing the rest of a
list (`[a b c ..rest]`) just like [insert favourite ML dialect]. Also includes
the ability to use predicates such as `($fn do_thing:int?i + i 1)`. This is
basically just so I can get parametric polymorphism without having to read a
book on Hindley-Milner type systems. This currently can't be implemented in
script-space macros - even if I implemented macros - like it is in lisp because
it's used by the runtime for...
* Function resolution based on patterns: so I really like being able to define
the same function with different guards or patterns in Haskell and dispatching
to different implementations based on value at runtime, but I quite like being
able to interpret my code line-by-line, so to allow that I implemented this
glorious hack where when looking for a function I will continuously step back
to earlier and earlier definitions until I reach one with a matching pattern.
This currently doesn't work when passing functions as arguments unless you do
some dirty hack like `map: ($fn a do_thing:a) [1 2 3]`. I'll hopefully
fix that in the future.

Features not yet implemented:
* Macros
* IO (you can print but that's it)

For more examples of code, check out `test.pfx`.
