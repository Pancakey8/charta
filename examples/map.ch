fn ▭1 (x) {
→ ▭
}

fn p (stk n) {
→ ⋮ ▭
}

fn map (f stk) {
↓              ↓ ⊡ ↷ ⇈ ↻ ▭1 ⊢! ↕ ←
↕             [≡]                ⇆
[≡]            ⊡                 -
⊡              ⊢!                1
⊢!             1                 ↑
→      ⇈ 0 >                     ? ∅ ↕ ∅
               ≠
               ?→ "Map expects 1↦1 function" ⊗
               ⊢!
               ↕
               ∅
       ↑ ↻ p ↻ ← 
}

fn ++ (n) {
→ 1 +
}

fn main () {
→ 1 2 3 4 5 ▭ [++] map ⇈ put ↓
                            [⇈] {# 1↦2 #}
                            map
                          {# ^ FAIL #}
}
