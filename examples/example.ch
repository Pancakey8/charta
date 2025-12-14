fn fibo (N) {
→ ⇈ 0 = ? ⇈ 1 = ? ⇈ 1 - fibo ↕ 2 - fibo +
        ↓       ↓
}

fn all1 (stack fn) {
→ ↕ [≡] sap ⊢! ⇈ 0 >                  ? ∅
                                      ↓
                                      1
               ↕                      -
               ↑ sap [⇓] sap ⇓ ⇈ ⇑ ↕  ←
}

fn main () {
→ 10 ⇈ 1 - ⇈ 0 > ? ▭ [fibo] all1 put
                 ↓
     ↑           ←
}
