use "io"
use "strings"

fn test-len () {
→ "hello" len 5 = ? 1 put
                  ↓
                 "OK"
                 put
}

fn test-pfx () {
→ "foobar" "foo" ¿pfx ¬ ? "hello" "xx" ¿pfx ? "hello" "qwerasdf" ¿pfx ? "OK" put
                        ↓                   ↓                         ↓
                        1                   2                         3
put                     ←                   ←                         ←
}

fn test-sfx () {
→ "foobar" "bar" ¿sfx ¬ ? "hello" "xx" ¿sfx ? "hello" "qwerasdf" ¿sfx ? "OK" put
                        ↓                   ↓                         ↓
                        1                   2                         3
put                     ←                   ←                         ←
}

fn test-split () {
→ "hey//hi//ho" "//" split ⊢! "hey" = ? 1 put
                                      ↓
                                      ⊢!
                                      "hi"
                                      =
                    put 3? = "ho" ⊢! ←?
                         ↓            → 2 put
                         ""
                         "asd"
                         split   → 4 put
                         [·]     ↑
                         → ⊡ ⊢!  ? "OK" put
}

fn main () {
↓
"test-len: "
∋
test-len

"test-pfx: "
∋
test-pfx

"test-sfx: "
∋
test-sfx

"test-split: "
∋
test-split
}