# Charta: Getting Started

Charta is a bit unusual. I will introduce two concepts here: Stack-based code and 2D code.

## Stack-based thinking

Most languages have the idea of an expression.

Some expressions are unary operations; `!x`, `-x`, `&x`, `*x`

Some are binary operations; `a + b`, `a && b`, `a % b`

And there are also special cases, like function calls; `open("file.txt", "r")`, `array[3]`.

---

In stack-based code, we consider a more uniform approach. We only have literals and functions:

Some literals are,

```
{# numbers #}
3
-10.5
{# characters & strings #}
#x
#\n
"Hello"
{# booleans #}
⊤ or T
⊥ or F
```

and functions are,

```
↕ or swp {# swap the top two elements of the stack #}
⇈ or dup {# duplicate the top element of the stack #}
⇆ or rev {# reverses the stack #}
+, -, *, / {# Basic operations are also functions #}

{# You can even define a function with 'special' symbols as names #}
{# Note: special in other languages, just another character here! #}
fn ++ (num) {# define your own increment function #}
```

Then, operations are applied in a single direction<sup>mostly, see: [2D](#2d-thinking)</sup>

```
2 3 + 5 *
```

can be interpreted as: `(2 + 3) * 5`; since we first push `2`, `3` on the stack,
then add them up, then push a `5` on the stack and multiply it with the sum.

Stack-based code allows for code that can operate at varying degrees of control:
You can operate on a singular element with `2 3 +`, or operate on multiple by
packing elements `▭` (rect function) and looping. You can even operate on
operations by pushing functions on the stack `[+]` (function values). Even with
these layers, the code never loses its homogeneous look and behaviour.

## 2D Thinking

One flaw of stack-based code is that it's really hard to make control flow that works right. Consider the below pseudocode

```
if (the stars align) {
  swap the top two elements on the stack
  increment top element by one
} else {
  multiply top element by two
}
print
```

Now, suppose the stack is initialised with `[ omens, luck ]` (written top to
bottom). If the stars align, we swap the order and increment our luck
`[ (luck + 1), omens ]`. Otherwise, we double our omens `[ (omens * 2), luck ]`.

Is this all the code does? No. Notice how after the branching behaviour, we have
further code (in this case, we print something). However, the two branches
**leave the stack misaligned**! One branch puts luck on top, the other puts the
omens on top! How can this bug be? It's because we assumed that a stack-based
language should be written linearly, like a procedural language. However,
stack-based code doesn't store values on a list of variables that you define
once, you instead write code dynamically, in one big chain.

To solve this problem, we can instead enforce a syntax that puts code in one
long chain. This makes it so that any two branching paths that lead to one
operation, clearly lead to one operation. This clarity is key to making sure
*monkey-brain* can understand that it needs to be careful

```
if (the stars align) { swap 1 + } { 2 * } print
```

Really nice, we can clearly see that `{ swap 1 + }` and `{ 2 * }` have to lead
to the same alignment. Now a trick is to observe alignment shifting
operators. `2 *` is does nothing to stack alignment, but `swap` is something that has
to be inverted by itself (i.e. `[a b] -swap> [b a] -swap> [a b]`). We now have
to consider this for the two branches.

```
if (the stars align) { swap 1 + swap } { 2 * } print
```

And our bug is fixed.

---

Charta takes this idea further by laying out branches and control flow in
2D. Since we can only express code in a single flow, why not make the flow bend
around and about? Above branching code can be rewritten in Charta syntax as below

```
→ star-alignment ?  2  *   → put
                 ↓
                 swp
                 → 1 + swp ↑
```

**WHAT?** Is it scary? Let's break it down:

### Directional execution

Execution starts at the top left of the total grid. By default, it moves right. The
first `→` is thus redundant, but good for clarity. Any arrows in code, like the
ones found in the small dip we see

```
→ 1 + swp ↑
```

redirect the execution flow. Execution continues in any direction until it exits
the grid, at which point it returns from the current function or scope. Spaces
and `{# comments #}` are treated transparently, meaning execution doesn't stop
on them.

However, things get a bit tricky when you consider looping or branching code. If
you ever hit a piece of code your execution has traversed before, you enter it
in the direction you originally evaluated it.
That is, code is **assigned a direction at first encounter.** Though this sounds
troublesome, it is actually very intuitive in practice, and leads to code that
looks like a flowchart.

So in short, trace the code starting top-left and you will have an idea of where
the exit points, loops, branches and tricky behaviour are.

### Branching operator

In the example above, we consider `star-alignment` to be a function that pushes
a boolean on top of the stack. However, Charta isn't strict in its typing, and
all values get a truthy and falsy property. A number, for example, is truthy;
but the number 0 is falsy. A string is truthy, but the empty string is again
falsy. The generalization is, if a type has a valid and sensible "null state",
that state is considered falsy.

Now, the next part of our execution is that we hit `?`. This is a special
branching operator. It pops the top value on the stack. If the value is falsy,
it does nothing. Otherwise, it branches perpendicularly. You may notice that at
any point, we have two perpendicular directions. If we were traversing
horizontally, we may branch up or down. If we were traversing vertically, we may
branch left or right. As such, you have to pick **only one of two directions** and
**immediately adjacent to the `?` operator, append an arrow in the chosen direction.**

In our example, just below `?` we have a `↓`, which tells us that we move
downwards if `star-alignment` returned true, and otherwise we keep move right.

Then, if we follow the two branches, we see that they meet before the `put`, our
print function. As such, we expect the small section that formed from the truthy
branch (looks like a mug handle) to act the same way as the straight path
leading to put.

The straight path has `2 *` which doesn't change stack alignment, and the
branching path has two `swp` which cancel out in total, meaning the branches do
not actually misalign.

## Coding for yourself

Charta requires -as you may have noticed- unicode characters to type. The most
basic form we have seen so far is the directional arrows `↑`, `↓`, `→`, `←`. If
you want to code Charta yourself, then you need a way to type these
characters. For that, most editors support TeX insertion / TeX snippets. If you
do enable TeX insertion in your editor, you can type the 4 arrows with
`\uparrow`, `\downarrow`, `\rightarrow`, `\leftarrow`.

A lot of common functions also get custom symbols, for example `swp` can also be
written `↕`, inserted with `\updownarrow`. The reason for the use of unicode is
the 2D nature of the language, in which spatial alignment matters. If verbose,
long function names were used everywhere, then code could end up blocking itself:

```
→ foo bar ?
          ↓
      thingamabob
      ↑   ←
```

In the above code for example, we cannot loop<sup>[(next section)](#roller-coaster)</sup> to `bar` as it is.
This is because thingamabob gets in the way of our traversal. If we move thingamabob out of the way however,

```
→ foo bar ?
          ↓
          thingamabob
      ↑   ←
```

then we make it harder to expand our code,

```
→ foo bar ? some-ops ↓
          ↓
          thingamabob
      ↑   ←    ↑     ←
```

Oops! The same situation all over. Unless you're writing trivial code, this is
unavoidable, thus short names for common operations is preferred.

## Roller coaster

Now let's introduce loops. Since we already *hopefully* understand the general
concepts, loops won't be very hard to analyse. Let's for example look at the
loopy code found in the README of the project.

```
fn main () {
→ 3 ⇈       0 > ? "Go!" put
    -           ↓
    1
    ↑ put ⇈     ←
}
```

The expected output is

```
3.0
2.0
1.0
Go!
```

Let's analyse carefully to figure out why that is.

### Functions

This snippet introduces something we haven't talked about yet, functions. A
function is declared with the `fn` keyword, followed by a name (`main`),
followed by a list of parameters `()`. For example, `+` may be thought of a function that takes two numbers

```
fn + (a b) {
...
}
```

A function takes the number of parameters it specifies and pops them from the
caller stack into its own stack. So, a function can isolate only the objects it
cares about from the rest of the stack, allowing you to write complex logic that
doesn't accidentally mess up the entire stack.

It should also be noted that the name of the parameter has no significance, and
is only written for documentation purposes.

A function, once execution exits, pushes its entire stack back onto the caller's stack.

And, as we explained prior, the grid of a function starts execution from the top-left, and by default moves right:

```
fn + (a b) {
@ <-- start here
                     ∨ definitely not here
@ <-- not here       @
}
```

### The new face

Alongside more traditional symbols like `-`, `>`; or ones we already recognize
now like `?` and the arrows, we see a new symbol in our example:

| Symbol              | Textual Name | Purpose                            | Mnemonic                                        |
|---------------------|--------------|------------------------------------|-------------------------------------------------|
| `⇈` (`\upuparrows`) | `dup`        | Duplicate top element of the stack | Double arrows pointing up resembles duplication |

### Breakdown of the example

First, we push a `3` on the stack.

Then, remember that whitespace is not important for traversal (it is
transparent). So, since we're moving right, we can imagine the entire line as
follows

```
⇈ 0 > ? "Go!" put
```

What does this do? It duplicates the top of the stack `x` and performs the
comparison `x > 0`. Currently, this gives us true, since `3 > 0`.

Since we have a truthy value on the stack, `?` branches, moving us down.

We do nothing going down, and simply hit a left arrow, which leads us to this line:

```
↑ put ⇈ ←
```

which should be read right-to-left based on direction, note that symbols do not
get reversed to avoid confusion (so put doesn't become tup!):

```
→ ⇈ put ↑
```

which simply duplicates the top of the stack and prints. What is at the top
currently? Let's trace it back.

- We had an initial state of `[3]`.
- We duplicated, `[3 3]`
- and compared with `0` `[⊤ 3]`
- and then we branched, popping the true. `[3]`

so duplicating and printing still gives us `[3]` but it also prints 3 on the
screen.

Then, following the line back up,

```
⇈
-
1
↑
```

which can be written horizontally as

```
→ 1 - ⇈
```

we can see that the line simply subtracts 1 from the top of the stack, leaving
us with `[2]`. Then, interestingly, the line crosses somewhere we have already
been. This means that we will follow the direction we initially did when
crossing this path, which was right (→). Then, this lets us complete a loop. We
again compare `>0`, print etc.

In short, after printing `3`, we will print `2`, and then `1`; but at `0`, we
will not branch at `?`  since `0 > 0` is false. Instead we will move forward,
and print go, then exit the `main` function and the program.

So, the output is

```
3.0
2.0
1.0
Go!
```

## What next?

Some concepts relating to stack and function operations haven't been covered
yet. You can find them in [Advanced Concepts](./Advanced.md).

If you want to, you can also check the [examples](../examples/) to get a better
idea of the language in action.

Obviously, we introduced very few functions here, and the examples use more than
that. However, the language has a full [cheatsheet](./Cheatsheet.md)! Do check
it out and use it as a reference.
