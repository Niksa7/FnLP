open System

let Div x func init=
    let rec DivRec x func init beg=
        if beg = 0 then init
        else
            let initmod = 
                if x % beg = 0 then func beg init 
                else init
            let newdiv = beg-1
            DivRec x func initmod newdiv
    DivRec x func init x

[<EntryPoint>]
let main argv =
    printfn "Введите значение x:"
    let x = Convert.ToInt32(Console.ReadLine())
    let resadd = Div x (fun a b -> a+b) 0
    let resmult = Div x (fun a b -> a*b) 1
    printfn "Сумма делителей числа x:"
    resadd |> printfn"%d"
    printfn "Произведение числа x:"
    resmult |> printfn"%d"
    0