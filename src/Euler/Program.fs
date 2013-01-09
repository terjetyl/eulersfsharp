// Learn more about F# at http://fsharp.net
module Program

open Xunit
open FsUnit.Xunit

module public Primes = 
    let sieve_primes top_number = 
        let numbers = [ yield 2L
                        for i in 3L..2L..top_number -> i ]
        let rec sieve ns = 
            match ns with
            | [] -> []
            | x::xs when x*x > top_number -> ns
            | x::xs -> x::sieve (List.filter(fun y -> y%x <> 0L) xs)
        sieve numbers 

    [<Fact>]
    let ``sum of 5 first numbers in sieve should be 28`` () =
        List.sum (sieve_primes 12L) |> should equal 28L

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

module Eulers3 = 
    let numbers = Primes.sieve_primes 600851475143L
    let answer = numbers.Tail
    printfn "Eulers 3 : %A" answer

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
    let numbers = Primes.sieve_primes 1999999L
    let answer = List.sum numbers
    printfn "Eulers 10 : %A" answer

module Eulers12 = 
    let factors number = seq {
        for divisor in 1 .. (float >> sqrt >> int) number do
        if number % divisor = 0 then
            yield divisor
            yield number / divisor
    }

module Eulers14 = 
    let oddfn = fun x -> x * 3 + 1;
    let evenfn = fun x -> x / 2;

    let mutable start = 13
    let mutable continueLooping = true

    let run startnumber = 
        
        start <- startnumber

        while continueLooping do
            if start % 2 = 0 then 
                start <- evenfn(start)
                printfn "%A" start
            else 
                start <- oddfn(start)
                printfn "%A" start

            if start < 2 then
                continueLooping <- false


        