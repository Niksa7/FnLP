open System

let rec fib n =
    match n with
    |1 |2 -> 1
    |_ -> fib (n-1) + fib(n-2)

let fib2 n =
    let rec loop acc1 acc2 n =
        match n with
        | 0 -> acc1
        | 1 -> acc2
        | _ ->
            loop acc2 (acc1 + acc2) (n - 1)
    loop 0 1 n