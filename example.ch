fn exit_routine {
> "finish" put
}

fn main {
> 100 dup 1 - |
              0
             less
       ^  put<?
              > exit_routine
}