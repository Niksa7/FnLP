open System

(*
1.13
Дан целочисленный массив. Необходимо разместить элементы,
расположенные до минимального, в конце массива.
*)

let rec readlistRec n =
    match n with
    |0->[]
    |_->
        let Head = Convert.ToInt32(Console.ReadLine())
        let Tail = readlistRec (n-1)
        Head::Tail

let rec writeListRec = function    
    |[]->0
    |h::t->
        printfn $"{h}"
        writeListRec t

let ListLenght list =
    let rec ListLenghtRec list count =
        match list with
        |[]->count
        |h::t->
            let newCount = count+1
            ListLenghtRec t newCount
    ListLenghtRec list 0

let Ind_of_Min list = 
    let rec Ind_of_Min_Rec list min indMin ind =
        match list with
        |[]->indMin
        |h::t ->
            let newMin = if h<min then h else min
            let newInd = if h<min then ind else indMin
            Ind_of_Min_Rec t newMin newInd (ind+1)
    Ind_of_Min_Rec list list.Head 0 0

let Before_min_to_end list = 
    let indexMin = Ind_of_Min list
    let rec Before_min_to_end_rec list indMin ind ListBefore ListAfter = 
        match list with 
        |[]-> ListAfter @ ListBefore
        |h::t ->
            let NewInd = ind+1
            if ind<indMin then 
                let NewList = ListBefore @ [h]
                Before_min_to_end_rec t indMin NewInd NewList ListAfter
            else 
                let NewList = ListAfter @ [list.Head]
                Before_min_to_end_rec t indMin NewInd ListBefore NewList
    Before_min_to_end_rec list indexMin 0 [] []


[<EntryPoint>]
let main argv =
    printfn "Кол-во элементов после максимального:"
    printfn $"Введите количество элементов: "
    let list = readlistRec (Convert.ToInt32(System.Console.ReadLine()))
    let newlist = Before_min_to_end list
    writeListRec newlist
    0