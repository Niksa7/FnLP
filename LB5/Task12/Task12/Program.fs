open System

[<EntryPoint>]
let main argv =

    let test answer =
        match answer with
        | "F#" | "Prolog" -> printfn "Подлиза"
        | _ -> printfn "Неплохой выбор!"

    printfn "Реализация композиции функций. \nКакой твой любимый язык программирования?"
    (Console.ReadLine >> test >> Console.WriteLine)()

    printfn "Реализация конвейера и каррирования. \nКакой твой любимый язык программирования?"
    Console.ReadLine() |> test |> Console.WriteLine

    0 