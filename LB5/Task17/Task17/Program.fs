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

    //init - счетчик делителей удовлетв. условию предиката
let Div x (func: int->bool) =
    let rec DivRec x func init beg=
        if beg = 0 then init
        else
            let initmod = 
                if x % beg = 0 && func beg then init+1
                else init
            let newdiv = beg-1
            DivRec x func initmod newdiv
    DivRec x func 0 x

let SimpDiv x (func: int->bool) =
    let rec SimpDividersRec x func init beg = 
        if beg = 0 then init
        else
            let initmod =
                if nod x beg = 1 && func beg then init+1
                else init
            let newdiv = beg-1
            SimpDividersRec x func initmod newdiv
    SimpDividersRec x func 0 x

[<EntryPoint>]
let main argv =
    printfn "Введите значение x:"
    let x = Convert.ToInt32(Console.ReadLine())
    let resdivpredict = Div x (fun a -> a>6)
    let ressimpdivpredict = SimpDiv x (fun a -> a>6)
    let resEuler = Euler x
    printfn "Эйлерово число:"
    resEuler |> printfn"%d"
    printfn $"Количество делитель числа {x} и удовлетворяющим условию a>6:"
    //пример: для числа 12 только 12
    resdivpredict |> printfn"%d"
    printfn $"Количество делителей числа {x} взаимно простых с ним и удовлетворяющим условию a>6:"
    //пример: для числа 12 эйлерово число = 4(1,5,7,11) в соответсвии с условием будут учтены только (7,11)
    ressimpdivpredict |> printfn"%d"
    0