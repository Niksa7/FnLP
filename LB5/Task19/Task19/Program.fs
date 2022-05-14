open System

//метод #1 - Найти сумму простых делителей числа.

let Prime_Divisor x=
    let rec Prime_Divisor_Rec x beg count=
        match beg with
        |_ when beg>x/2->count
        |_ when x%beg=0->
            let beg1=beg+1
            let count1=count+1
            Prime_Divisor_Rec x beg1 count1
        |_ -> 
            let beg1=beg+1
            Prime_Divisor_Rec x beg1 count
    Prime_Divisor_Rec x 2 0

let Sum_of_Divisors x=
    let rec Sum_of_Divisors_Rec x sum beg=
        match beg with
        |beg when beg>x/2->sum
        |beg when x%beg=0 && Prime_Divisor beg = 0->
            let sum1=sum+beg
            let beg1=beg+1
            Sum_of_Divisors_Rec x sum1 beg1
        |_-> 
            let beg1=beg+1
            Sum_of_Divisors_Rec x sum beg1
    Sum_of_Divisors_Rec x 0 2

//метод #2 - Найти количество нечетных цифр числа, больших 3.

let rec Is_odd_digit_Rec x =
    match x with
    |x when x % 2 = 1 -> true
    |_ -> false

let rec Is_greater_3_Rec x =
    match x with
    |x when x > 3 -> true
    |_ -> false

let Number_of_odd_digits_greater_3 x=
    let rec Number_of_odd_digits_greater_3_Rec x count=
        match x with
        |x when x=0 -> count
        |x when Is_odd_digit_Rec (x%10) && Is_greater_3_Rec (x%10) -> 
            let count1=count+1
            let x=x/10
            Number_of_odd_digits_greater_3_Rec x count1
        |_ -> 
            let x=x/10
            Number_of_odd_digits_greater_3_Rec x count
    Number_of_odd_digits_greater_3_Rec x 0

//метод #3 - Найти прозведение таких делителей числа, сумма цифр которых меньше, чем сумма цифр исходного числа.

//SumNum - возвращает сумму цифр числа
let SumNum x=
    let rec SumNumRec x sum=
        match x with
        |x when x=0->sum
        |_ -> 
            let sum=sum+(x%10)
            let x=x/10
            SumNumRec x sum
    SumNumRec x 0

let MultDiv x=
    let rec MultDivRec x mult beg=
        match beg with
        |beg when beg>x/2 -> mult
        |beg when x%beg=0 && (SumNum x) > (SumNum beg) -> 
            let mult1 = mult*beg
            let beg1=beg+1
            MultDivRec x mult1 beg1
        |_ ->
            let beg1=beg+1
            MultDivRec x mult beg1
    MultDivRec x 1 2

[<EntryPoint>]
let main argv =

    printfn "Введите значение x:"
    let x = Convert.ToInt32(Console.ReadLine())
    //метод #1
    let ressumsimple = Sum_of_Divisors x
    printfn $"Сумма простых делителей числа {x}:"
    ressumsimple |> printfn"%d"

    printfn "Введите значение x:"
    let x = Convert.ToInt32(Console.ReadLine())
    //метод #2
    let rescountodd = Number_of_odd_digits_greater_3 x
    printfn $"Количество нечетных цифр числа {x}, больших 3:"
    rescountodd |> printfn"%d"

    printfn "Введите значение x:"
    let x = Convert.ToInt32(Console.ReadLine())
    //метод #3
    let resmultdiv = MultDiv x
    printfn $"Прозведение таких делителей числа {x}, сумма цифр которых меньше, чем сумма цифр исходного числа:"
    resmultdiv |> printfn"%d"
    0