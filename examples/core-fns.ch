fn stack-ops () {
↓ {# test ⇈ ∅ ↕ ↻ ↷ ⊼ #}
→ 3 ⇈ 3 ≠ ? 3 ≠ ? ↓
          ↓     ↓ 1
          1     2 2
         put  put → 3 4 ∅ 3 ≠ ? ↕ 1 ≠ ?∅ 1 2 3 4 ↻ 2 ≠ ? ↓
                              ↓       ↓                ↓ ↷
                              3       4                5 3
                             put     put             put ≠
                                               put 6    ←?
                                                         → ⊼ 4 ≠ ? 1 ≠ ↕ 4 ≠ ∨ ? "OK" put
                                                                 ↓             ↓
                                                                 7             8
                                                                 put           put
}

fn stack-ops-2 () {
↓ {# test ⇑ ⇓ ⇆ ≡ · ▭ ⊢ ⊩ ⊣ ⊢! ⊣! ⋮ #}                                         → = ? 9 put
                                                                               3   ↓
                                                                               ⊣!  ⋮
→ 1 2 3 4 5 ⇑ 1 ≠ ? ⇓ 4 ≠ ? ⇆ 5 ≠ ? ≡ 2 ≠ ? · ? ▭ ⊢ 2 ≠ ? ⊩ ↕ ⊣ ↻ ≠ ? ⊢! 2 ≠ ? ↑   ·
                  ↓       ↓       ↓       ↓   ↓         ↓           ↓        ↓     ?→ "OK" put
                  1       2       3       4   5         6           7        8     10
put               ←       ←       ←       ←   ←         ←           ←        ←     ←
}

fn arithmetics-comparisons () {
↓ {# test + - * / = ≠ < > ∧ ¬ ⊤ ⊥ #}
→ 2 3 + 5 * 25 ≠ ? 15 2 * 2 - 8 / 3.5 ≠ ? 35.5 2 % 1.5 ≠ ? 2 3 < 6 5 > "asd" "asd" = ∧ ∧ ¬ ? ⊤ ⊥ ∧ ? "OK" put
                 ↓                      ↓                ↓                                 ↓       ↓
                 1                      2                3                                 4       5
put              ←                      ←                ←                                 ←       ←
}

fn conversions () {
↓ {# test str num bool ord chr ▭s #}
→ 13.5 str "13.5" ≠ ? "27" num 27 ≠ ? "" bool ? #A ord 65 ≠ ? 90 chr #Z ≠ ? 1 2 ↓
                    ↓               ↓         ↓             ↓             ↓     #o
                    1               2         3             4             5     #l
put                 ←               ←         ←             ←             ←     #l            put
                                                                                #e             6
                                                                                #H             ↑
                                                                                → ▭s "Hello" ≠ ? "OK" put
}

fn types () {
↓ {# test ¿str ¿num ¿bool ¿char ¿stk ¿fn #}
→ "foo" ¿str ¬ ? 3 ¿num ¬ ? ⊤ ¿bool ¬ ? #X ¿char ¬ ? ▭ ¿stk ¬ ? [+] ¿fn ¬ ? "OK" put
               ↓          ↓           ↓            ↓          ↓           ↓          
               1          2           3            4          5           6
put            ←          ←           ←            ←          ←           ←
}

fn mixed-fns () {
→ 2 3 [+] ∘ 5 ≠ ? 3 2 1 ▭ [+] ⊡ [*] ⊡ ⊢! 9 ≠ ? "OK" put
                ↓                            ↓
                1                            2
put             ←                            ←
}

fn main () {
↓
"stack-ops:"
put
stack-ops
"stack-ops-2:"
put
stack-ops-2
"arithmetics-comparisons:"
put
arithmetics-comparisons
"conversions:"
put
conversions
"types:"
put
types
"mixed-fns:"
put
mixed-fns
}