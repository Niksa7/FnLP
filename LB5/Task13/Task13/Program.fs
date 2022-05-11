open System

//Рекурсия вверх
let rec multrec x = 
    match x with
    |0 -> 1
    |x -> (x%10)*multrec (x/10)

//хвостовая рекурсия
let Multhvost x =
    let rec Mult x c=
        if x=0 then c
        else  Mult (x/10) (c*(x%10))
    Mult x 1

let MaxUp x =
    let rec MaxUpRec x = 
        match x with
        |0 -> 0
        |_ -> 
            if MaxUpRec(x/10) < x%10 then x%10 
            else MaxUpRec(x/10)
    MaxUpRec x

let MinUp x =
    let rec MinUpRec x =
        if x < 10 then x
        else min (x % 10) (MinUpRec (x / 10))
    MinUpRec x

[<EntryPoint>]
let main argv =
    let a = Convert.ToInt32(Console.ReadLine())
    Console.WriteLine("Минимальная цифра(рекурсия вверх):")
    a |> MinUp |> printfn "%d"
    Console.WriteLine("Произведение цифр числа(рекурсия вверх):")
    a |> multrec |> printfn "%d"
    Console.WriteLine("Произведение цифр числа(хвостовая рекурсия):")
    a |> Multhvost |> printfn "%d"
    Console.WriteLine("Максимальная цифра(рекурсия вверх):")
    a |> MaxUp |> printfn "%d"
    0 
