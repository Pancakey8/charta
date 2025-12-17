use "io"

fn words (str) {
↓ ↓                                     ←
→ [·] ⊡ ⊢! ? ⊢! ⇈ #  ≠ ? ∅ ⇓ ▭s [⇆] ⊡ ⇑ ↑ {# loop until the string is empty #}
           ↓           ↓
          {# loop until space, join into word #}
   ↑                 ↕ ←
           ∅
           ¿char
           ?→ ▭s [⇆] ⊡ {# handle final word #}
}

fn pigLatin (...) {
↓  ↓  ↕                                      ←
→ [·] ⊡ ⊢! ? ⊢! [⇆] ⊡ [⇑] ⊡ ⋮ #a #y ▭s [⇆] ⊡ ↑
           ↓ {# for each word, move first letter to the end, #}
           ∅ {# and add suffix "ay #}
}

fn unwords (...) {
↓  ↓ ↕                            ←
→ [≡] ⊡ ⊢! 1 = ? ⊢! ⋮ #  ▭s [⇓] ⊡ ↑
               ↓ {# loop for each word except last and add space #}
               ⊢!
               ↕
               ∅ {# concatenate into one sentence #}
               → ≡ 1 > ?
                       ↓
                       +
                 ↑     ←
}

fn main () {
→ "man is condemned to be free" words pigLatin unwords put
}