open System


let rec readlistRec n =
    match n with
    |0->[]
    |_->
        let Head = Convert.ToInt32(Console.ReadLine())
        let Tail = readlistRec (n-1)
        Head::Tail

let rec writeListRec = function    
    |[]->0
    |Head::Tail->
        printfn $"{Head}"
        writeListRec Tail

let Max_on_interval list (a,b) =
    let rec Max_on_interval_Rec list (a,b) max =
        match list with
        |[]->max
        |h::t ->
            let newMax =
                if h>max && h>a && h<b then h else max
            Max_on_interval_Rec t (a,b) newMax
    Max_on_interval_Rec list (a,b) a


[<EntryPoint>]
let main argv =
    printfn $"Введите границы интервала: "
    let interval = (Convert.ToInt32(Console.ReadLine()),Convert.ToInt32(Console.ReadLine()))
    printfn $"Введите количество элементов: "
    let list = readlistRec (Convert.ToInt32(Console.ReadLine()))
    let x = Max_on_interval list interval
    Console.WriteLine(x)
    0