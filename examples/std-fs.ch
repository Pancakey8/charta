use "io" as io

fn put (s) {
→ io.put
}

use "fs"

fn main () {
→ "std-fs.txt" ⨆w "Hello world\n"     ∋ × ↓
                                        "std-fs.txt"
                                          ⨆r
                                          sz
                                          12
                                          ≠
                                  put 1  ←?
                                          ∈
                                          #H
           ←                              ≠
          put                       ↑ 2  ←?
          "OK"                      3
           ↑                        ↑    tell
     put 4 ? = "ello world\n" slurp ? ≠ 1 ←
}