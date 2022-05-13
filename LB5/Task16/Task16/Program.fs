open System

let rec nod a b =
    if a % b = 0 then b
    else
        nod b (a%b)

let Euler x =
    let rec EulerFunc x func init beg =
        if beg = 0 then init
        else
            let initmod =
                if nod x beg = 1 then func init 0
                else init
            let newdiv = beg-1
            EulerFunc x func initmod newdiv
    EulerFunc x (fun x y->x+1) 0 x

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
    let resEuler = Euler x
    printfn "Сумма делителей числа x взаимно простых с ним:"
    resadd |> printfn"%d"
    printfn "Произведение делителей числа x взаимно простых с ним:"
    resmult |> printfn"%d"
    printfn "Эйлерово число:"
    //пример: для числа 6 эйлерово число = 2, т.к. только 1 и 5 взаимно простые с 6
    resEuler |> printfn"%d"
    0 