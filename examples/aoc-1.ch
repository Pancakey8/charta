fn lines' (str) {
↓ {# Break if empty #}          {# Collect current line #}
                                {# Continue recursively #}
→ [≡] sap ⊢! 0 = ? ⊢! ⇈ #\n ≠ ? ∅ ⇆ ▭s ↕ lines'
                 ↓            ↓
                 ∅            ↕
                   ↑          ←
             {# Loop until newline #}
}

fn lines (str) {
↓ {# Wrapper around recursion #}
→ lines' ⇆ ▭
}

fn mkRot (str) {
→ ⋮ #R = ? {# Assume left #} ▭s num -1 *
         ↓

         → ▭s num
}

fn rotLines (str) {
↓ {# Loop for each line #}
→ [≡] sap ⊢! →           ⇈ 0 >      ? ∅
                                    ↓
                                    1
                                    -
             ↑ ↕ ⊡ [⇓] ⊡ [mkRot] ↕  ←
}

fn solve (rotations current count) {
↓                  {# Sum top mod 100 #}                       
→ [≡] sap ⊢! 0 = ? ⊢! ↻   + 100 % ⇈ 0 = ?      → ↕ solve
                 ↓                      ↓
                 ∅                      ↻ {# Inc. count if current=0 #}
                 ∅                      1
   {# Exit #}                           →+ ↻ ↻ ↑
}

use "aoc-1-input"

fn main () {
→ input lines rotLines 0 50 ↻ solve "Answer:" put put
}