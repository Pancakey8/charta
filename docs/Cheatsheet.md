# Charta: Cheatsheet

Full language syntax:

| Symbol                             | Purpose                                                      |
|------------------------------------|--------------------------------------------------------------|
| `←` (`\leftarrow`)                 | Redirect execution left                                      |
| `→` (`\rightarrow`)                | Redirect execution right                                     |
| `↑` (`\uparrow`)                   | Redirect execution up                                        |
| `↓` (`\downarrow`)                 | Redirect execution down                                      |
| `?`                                | Branch perpendicularly if the top of the stack is truthy     |
| `3`, `-5`, `30.5`                  | Number literals.                                             |
| `#A`, `#ü`                         | Character literal. Can use escape codes (`#\n` = newline)    |
| `"Hello"`                          | String literal. Can use escape codes                         |
| `[+]`                              | Function value. Pushes function to stack instead of calling. |
| `fn <name> ( <args>* ) { <body> }` | Function definition                                          |
| `use "<file>" [as <namespace>]`    | Import `file.ch`, optionally under `namespace.`              |

The arrow characters, `?`, `#`, `"`, and all parenthesis types `(){}[]` are
reserved and cannot be used in symbol names.

A full list of builtin functions:

| Symbol                    | Text Name | Purpose                                                                                                                            |
|---------------------------|-----------|------------------------------------------------------------------------------------------------------------------------------------|
| `⇈` (`\upuparrows`)       | `dup`     | Duplicate top element of the stack                                                                                                 |
| `∅` (`\emptyset`)         | `drp`     | Removes top element of the stack                                                                                                   |
| `⊢` (`\vdash`)            | `fst`     | Copies the top element of a stack object to current stack                                                                          |
| `⊩` (`\Vdash`)            | `snd`     | Copies the second element of a stack object to current stack                                                                       |
| `⊣` (`\dashv`)            | `lst`     | Copies the bottom element of a stack object to current stack                                                                       |
| `⊢!` (`\vdash !`)         | `pop`     | Pops the top element of a stack object to current stack                                                                            |
| `⊩!` (`\Vdash !`)         | `hop`     | Pops the second element of a stack object to current stack                                                                         |
| `⊣!` (`\dashv !`)         | `bot`     | Pops the bottom element of a stack object to current stack                                                                         |
| `↻` (`\circlearrowright`) | `rot`     | Moves third element to the top, i.e. `[a b c] -> [c a b]`                                                                          |
| `↷` (`\curvearrowright`)  | `rot-`    | Move the top element to the third position, i.e. `[a b c] -> [b c a]`                                                              |
| `↕` (`\updownarrow`)      | `swp`     | Swaps the order of the topmost two elements, i.e. `[a b] -> [b a]`                                                                 |
| `⊼` (`\barwedge`)         | `ovr`     | Copies the second element to the top, i.e. `[a b] -> [b a b]`                                                                      |
| `▭` (`\rect`)             | `pack`    | Creates a new stack, pushes the current stack to the new stack as a single stack object                                            |
| `⋮` (`\vdots`)            | `spt`     | Expands a stack object into current stack                                                                                          |
| `≡` (`\equiv`)            | `dpt`     | Pushes the depth of the current stack on the stack                                                                                 |
| `·` (`\cdot`)             | `null`    | Pushes true to the stack if it's empty, false otherwise                                                                            |
| `⇆` (`\leftrightarrows`)  | `rev`     | Reverses the stack, so that the bottom element is now at the top                                                                   |
| `⇓` (`\Downarrow`)        | `shv`     | Moves the top element of the stack to the bottom                                                                                   |
| `⇑` (`\Uparrow`)          | `brg`     | Moves the bottom element of the stack to the top                                                                                   |
| `+`                       |           | Adds top two numbers, or if both are strings, concatenates                                                                         |
| `-`                       |           | Subtracts top two numbers                                                                                                          |
| `*`                       |           | Multiplies top two numbers, or if one is a string, repeats the string (`3 "a" *` equals `"aaa"`)                                   |
| `/`                       |           | Divides top two numbers                                                                                                            |
| `%`                       |           | Takes the remainder of the top number when divided by the second number on the stack                                               |
| `⊤` (`\top`)              | `T`       | Pushes true to the stack                                                                                                           |
| `⊥` (`\bot`)              | `F`       | Pushes false to the stack                                                                                                          |
| `=`                       |           | Pops and compares top two objects, pushing true if equal, otherwise pushes false                                                   |
| `≠` (`\neq`)              | `!=`      | Pops and compares top two objects, pushing false if equal, otherwise pushes true                                                   |
| `<`                       |           | Pops and compares top two numbers, pushing true if the top is less than the second, otherwise pushes false                         |
| `>`                       |           | Pops and compares top two numbers, pushing true if the top is greater than the second, otherwise pushes false                      |
| `≤` (`\leq`)              | `<=`      | Pops and compares top two numbers, pushing true if the top is less than or equal to the second, otherwise pushes false             |
| `≥` (`\geq`)              | `>=`      | Pops and compares top two numbers, pushing true if the top is greater  than or equal to the second, otherwise pushes false         |
| `∧` (`\wedge`)            | `&&`      | Pops and takes the logical and of top two booleans                                                                                 |
| `∨` (`\vee`)              | `||`      | Pops and takes the logical or of top two booleans                                                                                  |
| `¬` (`\neg`)              | `!`       | Pops and takes the logical negation of the top boolean                                                                             |
| `str`                     |           | Converts top element to a string                                                                                                   |
| `num`                     |           | Converts top element to a number                                                                                                   |
| `bool`                    |           | Converts top element to a boolean                                                                                                  |
| `ord`                     |           | Converts top character to a number                                                                                                 |
| `chr`                     |           | Converts top number to a character                                                                                                 |
| `▭s` (`\rect s`)          | `packs`   | Starting from the top of the stack, packs as many characters as possible into a string. The top element is the start of the string |
| `¿str`                    | `isStr`   | If the top element is a string, pushes true, otherwise pushes false. Does not consume                                              |
| `¿num`                    | `isNum`   | If the top element is a number, pushes true, otherwise pushes false. Does not consume                                              |
| `¿bool`                   | `isBool`  | If the top element is a boolean, pushes true, otherwise pushes false. Does not consume                                             |
| `¿char`                   | `isChar`  | If the top element is a character, pushes true, otherwise pushes false. Does not consume                                           |
| `¿stk`                    | `isStk`   | If the top element is a stack, pushes true, otherwise pushes false. Does not consume                                               |
| `put`                     |           | Pops and prints the top element of the stack, followed by a newline                                                                |
| `⚠` (`\warning`)          | `dbg`     | Debug prints the entire stack without consuming                                                                                    |
| `∘` (`\circ`)             | `ap`      | Calls the top function value, s.t. `f` and `[f] ∘` are equivalent                                                                  |
| `⊡` (`\dotsquare`)        | `sap`     | Expects the stack to have a function value followed by a stack object. Applies the function within the stack object                |

*Note: When types are mentioned, conversion is attempted if possible. E.g. `2 3 ∧` evalutes to true since `2` and `3` are truthy.*
