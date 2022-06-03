open System

//Чтение
let rec readlistRec n =
    match n with
    |0->[]
    |_->
        let Head = Convert.ToInt32(Console.ReadLine())
        let Tail = readlistRec (n-1)
        Head::Tail


//Запись
let rec writeListRec = function    
    |[]->0
    |h::t->
        printfn $"{h}"
        writeListRec t

//Индекс минимального элемента
let Ind_of_Min list = 
    let rec Ind_of_Min_Rec list min indMin ind =
        match list with
        |[]->indMin
        |h::t ->
            let newMin = if h<min then h else min
            let newInd = if h<min then ind else indMin
            Ind_of_Min_Rec t newMin newInd (ind+1)
    Ind_of_Min_Rec list list.Head 0 0


[<EntryPoint>]
let main argv =
    printfn "Индекс минимального:"
    printfn $"Введите количество элементов: "
    let list = readlistRec (Convert.ToInt32(System.Console.ReadLine()))
    let x = Ind_of_Min list
    printfn $"{x}"
    0
