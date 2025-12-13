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
→ ⋮ ▭ lines' ⇆ ▭
}

fn mkRot (str) {
→ ⋮ #R = ? {# Assume left #} ▭s num -1 *
         ↓

         → ▭s num
}

fn rotLines (str) {
↓ {# Loop for each line #}
→ [≡] sap ⊢! →               ⇈ 0 >      ? ∅
                                        ↓
                                        1
                                        -
             ↑ ↕ sap [⇓] sap [mkRot] ↕  ←
}

fn solve (rotations current count) {
→ ▭ ⇈ ⋮ "\nROT:" put put "CURRENT:" put put "COUNT:" put put ⋮ ↓ {# Print steps #}
↓                  {# Sum top mod 100 #}                       ←
→ [≡] sap ⊢! 0 = ? ⊢! ↻   + 100 % ⇈ 0 = ?      → ↕ solve
                 ↓                      ↓
                 ∅                      ↻ {# Inc. count if current=0 #}
                 ∅                      1
   {# Exit #}                           →+ ↻ ↻ ↑
}

fn main () {
↓
"L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82\n"
→ lines rotLines 0 50 ↻ solve "\nAnswer:" put put
}