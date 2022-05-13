open System

//Натуральные числа называют взаимно простыми, если их наибольший общий делитель равен 1.
let rec nod a b =
    if a % b = 0 then b
    else
        nod b (a%b)

let SimpDividers x func init =
    let rec SimpDividersRec x func init beg = 
        if beg = 0 then init
        else
            let initmod =
                if nod x beg = 1 then func beg init
                else init
            let newdiv = beg-1
            SimpDividersRec x func initmod newdiv
    SimpDividersRec x func init x

[<EntryPoint>]
let main argv =
    printfn "Введите значение x:"
    let x = Convert.ToInt32(Console.ReadLine())
    let resadd = SimpDividers x (fun a b -> a+b) 0
    let resmult = SimpDividers x (fun a b -> a*b) 1
    printfn "Сумма делителей числа x взаимно простых с ним:"
    resadd |> printfn"%d"
    printfn "Произведение делителей числа x взаимно простых с ним:"
    resmult |> printfn"%d"
    0 
 

