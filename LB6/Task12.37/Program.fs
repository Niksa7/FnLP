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

let IndLessNeigh (list:'int list) = 
  let rec IndLessNeighRec list left (ind:int) count=
      match list with
      |[] -> count
      |h::t->
        let newCount = 
            if h<left then count+1
            else count
        let newLeft = h
        if h<left then Console.WriteLine("{0}",ind)
        IndLessNeighRec t newLeft (ind+1) newCount
  IndLessNeighRec list.Tail list.Head 1 0


[<EntryPoint>]
let main argv =
    printfn $"Введите количество элементов: "
    let list = readlistRec (Convert.ToInt32(Console.ReadLine()))
    list|>IndLessNeigh|>Console.WriteLine
    0