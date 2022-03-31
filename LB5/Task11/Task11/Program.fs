open System

let test answer =
    match answer with
    | "F#" -> printfn "Подлиза"
    | "Prolog" -> printfn "Подлиза"
    | _ -> printfn "Неплохой выбор!"

[<EntryPoint>]
let main argv =
    printfn "Какой твой любимый язык программирования?"
    let word = Console.ReadLine()
    test word
    0 