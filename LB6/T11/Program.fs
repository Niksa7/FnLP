open System

(*
1.1
Дан целочисленный массив. Необходимо найти количество
элементов, расположенных после последнего максимального.
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

let ChangeList list func = 
    let rec ChangeList_rec list func new_list = 
        match list with
        | []->new_list
        | h::t->
            let first = h // First element
            let second = if t > []then t.Head else 1 //  Second element
            let third = if t > [] then (if t.Tail > [] then t.Tail.Head else 1) else 1 // Third element
            let sum_of_3 = func first second third // Sum of elements
            let new_list_1 = new_list@ [sum_of_3]// Add to list
            let mod_list = if t > [] then (if t.Tail > [] then t.Tail.Tail else []) else [] // происходит сдвиг списка на 2 элемента
            ChangeList_rec mod_list func new_list_1
    ChangeList_rec list func []



[<EntryPoint>]
let main argv =
    printfn $"Введите количество элементов: "
    let list = readlistRec (Convert.ToInt32(System.Console.ReadLine()))
    let new_list = ChangeList list (fun a b c -> a+b+c)
    printfn $"Результат: "
    writeListRec new_list
    0