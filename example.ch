fn exit_routine {
> "finish" put
}

fn main {
> 10 1 swap - dup 0 less ? exit_routine
                         |
                        dup
     ^                put<
}
