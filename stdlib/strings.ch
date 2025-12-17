fn len (str) {
→ [≡] ⊡ ⊢!
}

fn is-pfx (pfx str) {
→ ⊼ ⊼ len ↻ len ↻ ≥ ? ∅ ∅ ⊥
                    ↓
                    ↕
                   len            ←
                    0
                    =             ↕
             ⊤ ∅ ∅ ←?             ↑
                    → ⊢! ↻ ⊢! ↻ = ? ∅ ∅ ⊥
}

fn ¿pfx (pfx str) {
→ is-pfx
}

fn is-sfx (sfx str) {
→ ⊼ ⊼ [⇆] ⊡ ↕ [⇆] ⊡ ↕ is-pfx ↷ ∅ ∅
}

fn ¿sfx (sfx str) {
→ is-sfx
}

hide fn apd (stk v) {
→ ⋮ ▭
}

hide fn drp (stk n) {
→ ↕ ⇈ 0 > ? ∅
          ↓
    -     ↕
    1     ⊢!
    ↑ ↕ ∅ ←
}

fn split (sep str) {
↓                → ∅ ∅ ⇆ ▭
                 ↑
→ "" ↷ ↕ len 0 = ? ↕ ¿pfx ? ↻ ↻ ⊢! ↻ apd ↷ ↓
                          ↓     
          ↑                                ←
                         len      
                          ↻
                         drp
       ↑           ↷ "" ↕ ←
}
