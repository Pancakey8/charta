fn generics (x y) {
→ ∅
}

fn arithmetic (a) {
→ 3 + 5 *
}

fn branches-types (a) {
→ ¿char ? ¿int ? "hey"
        ↓      ↓
        ∅      1
               +
}

fn loop-ok (x) {
→ ¿int ?
       ↓
       1
       +
  ↑    ←
}

fn loop-fail (x) {
→ ¿int ? {# Can't take ints #}
       ↓
       ⇈
  ↑    ←
}

fn recursive-ok (x) {
→ 1 - recursive-ok
}

fn recursive-fail (x) {
→ 1 - ⇈ recursive-fail
}
