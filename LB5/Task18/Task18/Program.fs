﻿open System

(*«Работа с числами». Составить 3 функции для работы с
цифрами или делителей числа на основании варианта с использованием
только хвостовой рекурсии. Каждый метод отдельный коммит.
Вариант № 1
Метод 1 Найти сумму простых делителей числа.
Метод 2 Найти количество нечетных цифр числа, больших 3
Метод 3 Найти прозведение таких делителей числа, сумма цифр
которых меньше, чем сумма цифр исходного числа.*)

//метод #1 - Найти сумму простых делителей числа.

let SimpleDiv x=
    let rec SimpleDivRec x beg count=
        match beg with
        |_ when beg>x/2->count
        |_ when x%beg=0->
            let beg1=beg+1
            let count1=count+1
            SimpleDivRec x beg1 count1
        |_ -> 
            let beg1=beg+1
            SimpleDivRec x beg1 count
    SimpleDivRec x 2 0

let SumDiv x=
    let rec SumDivRec x sum beg=
        match beg with
        |beg when beg>x/2->sum
        |beg when x%beg=0 && SimpleDiv beg = 0->
            let sum1=sum+beg
            let beg1=beg+1
            SumDivRec x sum1 beg1
        |_-> 
            let beg1=beg+1
            SumDivRec x sum beg1
    SumDivRec x 0 2

//метод #2 - Найти количество нечетных цифр числа, больших 3.

let CountOddNumbers x=
    let rec CountOddNumbersRec x count=
        match x with
        |x when x=0 -> count
        |x when (x%10)%2=1 && (x%10)>3 -> 
            let count1=count+1
            let x=x/10
            CountOddNumbersRec x count1
        |_ -> 
            let x=x/10
            CountOddNumbersRec x count
    CountOddNumbersRec x 0

[<EntryPoint>]
let main argv =

    printfn "Введите значение x:"
    let x = Convert.ToInt32(Console.ReadLine())
    //метод #1
    let ressumsimple = SumDiv x
    printfn $"Сумма простых делителей числа {x}:"
    ressumsimple |> printfn"%d"

    printfn "Введите значение x:"
    let x = Convert.ToInt32(Console.ReadLine())
    //метод #2
    let rescountodd = CountOddNumbers x
    printfn $"количество нечетных цифр числа {x}, больших 3:"
    rescountodd |> printfn"%d"
    0