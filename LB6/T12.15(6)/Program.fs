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
    |h::t->
        printfn $"{h}"
        writeListRec t

//поиск локального минимума
let LocalMin Indmin list = 
    let rec LocalMinRec list Indmin Element Ind = 
        match list with
        |[]->true //если лок миним - последний элемент
        |h::t->
            let result=
                if (Ind+1=Indmin || Ind=Indmin) then
                    if (Ind=Indmin) then LocalMinRec t Indmin h (Ind+1) 
                    else
                        if (h>=t.Head && Ind+1=Indmin) then LocalMinRec t Indmin t.Head (Ind+1)
                        else false
                else               
                    if (Ind-1=Indmin && h>=Element) then true
                        else false
            if (Ind>=Indmin-1 && Ind<=Indmin+1) then result
            else
                LocalMinRec t Indmin Element (Ind+1)
    LocalMinRec list Indmin 0 0 


[<EntryPoint>]
let main argv =
    printfn $"Введите индекс локал минимума: "
    let ind = Convert.ToInt32(Console.ReadLine())
    printfn $"Введите количество элементов: "
    let list = readlistRec (Convert.ToInt32(Console.ReadLine()))
    let x = LocalMin ind list
    Console.WriteLine(x)
    0