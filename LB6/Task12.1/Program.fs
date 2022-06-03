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

let ListLenght list =
    let rec ListLenghtRec list count =
        match list with
        |[]->count
        |h::t->
            let newCount = count+1
            ListLenghtRec t newCount
    ListLenghtRec list 0

let Index_of_Max list =
    let rec Index_of_MaxRec list Max indMax ind =
        match list with
        |[]->indMax
        |h::t ->
            let newMax = if h>=Max then h else Max
            let newIndMax = if h>=Max then ind else indMax
            Index_of_MaxRec t newMax newIndMax (ind+1)
    Index_of_MaxRec list list.Head 0 0

//колво после последнего максимального
let Count_after_Max list =
    let IndexMax = Index_of_Max list
    let result = ListLenght list-IndexMax-1
    result


[<EntryPoint>]
let main argv =
    printfn "Кол-во элементов после максимального:"
    printfn $"Введите количество элементов: "
    let list = readlistRec (Convert.ToInt32(System.Console.ReadLine()))
    let x = Count_after_Max list
    printfn $"{x}"
    0