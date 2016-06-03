# Polish prefix notation-based language

W/ hints of Lisp, to taste.

There are just two kinds of function: unary and binary. To receive more
arguments one must take a list. Unary functions are applied with no whitespace,
binary functions are applied with whitespace. Macros are denoted by a `$` and
simply consume expressions until a matching closing brace or a `;`. Functions,
are a string of punctuation characters, but other expressions can be used as
functions by appending a `:` (no whitespace).

Example:

```
print:"Hello, world!";

$def do_thing a b
    + a b;
$def (%) fn a (
    print:fn;
    fn:a
);
% (-) do_thing: 1 2
```

Prints

```
Hello, world!
BuiltinUnaryFn(0x56233fb31d70)
```

and outputs -3
