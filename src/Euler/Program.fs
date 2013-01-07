// Learn more about F# at http://fsharp.net

module Primes = 
    let rec sieve = function
        | (p::xs) -> p :: sieve [ for x in xs do if x % p > 0 then yield x ]
        | []      -> []

module Eulers1 = 

    // create the list of nubers from 1 to 1000
    let numbers = [1 .. 999];

    // get numbers moddable with 3 or 5
    let r1 = List.filter (fun x -> x % 3 = 0 || x % 5 = 0) numbers

    // sum up numbers
    let answer = List.sum r1

    // print answer
    printfn "Eulers 1 : %A" answer


module Eulers2 = 

    // calculates the value of the item in the Fibonacci seq at the given position
    let rec Fib(n) = 
        if (n < 2) then
            1
        else
            Fib(n-2) + Fib(n-1)

    let FibSeq = Seq.unfold (fun (a, b) -> Some( a, (b, a + b) ) ) (1, 2)

    let answer = 
        FibSeq
            |> Seq.filter (fun x -> x % 2 = 0)
            |> Seq.takeWhile (fun x -> x <= 4000000)
            |> Seq.sum

    printfn "Eulers 2 : %A" answer


module Eulers6 = 
    let numbers = [1..100];

    let square = fun x -> x * x

    // list with the square of the first 100 numbers
    let list = List.map square numbers

    let sumofsquares = List.sum list

    let summednumbers = List.sum numbers

    let answer = square(summednumbers) - sumofsquares

    printfn "Eulers 6 : %A" answer

module Eulers10 = 
    let numbers = Primes.sieve [2..1999999];
    let answer = List.sum numbers
    printfn "Eulers 10 : %A" answer