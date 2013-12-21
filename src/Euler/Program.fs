// Learn more about F# at http://fsharp.net
module Program

open Xunit
open FsUnit.Xunit
open System
open System.Globalization

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

    let dividableBy3Or5 (x) = (x % 3 = 0) || (x % 5 = 0)

    let answer = [1..999] |> Seq.filter dividableBy3Or5 |> Seq.sum

    // print answer
    printfn "Eulers 1 : %A" answer


module Eulers2 = 

    // calculates the value of the item in the Fibonacci seq at the given position
    let rec Fib(n) = 
        if (n < 2) then
            1
        else
            Fib(n-2) + Fib(n-1)

    let evenNumbers (x) = x % 2 = 0

    let FibSeq = Seq.unfold (fun (a, b) -> Some( a, (b, a + b) ) ) (1, 2)

    let answer = 
        FibSeq
            |> Seq.takeWhile (fun x -> x <= 4000000)
            |> Seq.filter evenNumbers
            |> Seq.sum

    printfn "Eulers 2 : %A" answer

module Eulers3 = 

    let isPrime (possiblePrime:int64) =
        let sqrRootOfPrime = int64(System.Math.Sqrt(float(possiblePrime)))
             
        {1L .. sqrRootOfPrime}
        |> Seq.forall(fun divisor -> 
            match divisor with
            | 1L -> true
            | x when divisor = possiblePrime -> true
            | _ -> possiblePrime % divisor > 0L)

    let findFactors ofNumber =
        {2L .. (ofNumber/2L)}
        |> Seq.filter(fun testNumber -> ofNumber % testNumber = 0L)
        |> Seq.map(fun factor1 -> (factor1, (ofNumber / factor1)))
        |> Seq.takeWhile(fun (factor1, factor2) -> factor1 <= factor2)
        |> Seq.fold (fun acc (factor1, factor2) -> factor1 :: factor2 :: acc) []
        |> Seq.sort
        |> Seq.toList
        |> List.rev

    let sieve_primes top_number = 
        let numbers = [ yield 2L
                        for i in 3L..2L..top_number -> i ]
        let rec sieve ns = 
            match ns with
            | [] -> []
            | x::xs when x*x > top_number -> ns
            | x::xs -> x::sieve (List.filter(fun y -> y%x <> 0L) xs)
        sieve numbers 

    let answer = findFactors 600851475143L |> Seq.filter isPrime |> Seq.toList |> List.rev |> Seq.last
    printfn "Eulers 3 : %A" answer

module Eulers4 = 

    let rev str =
        let si = StringInfo(str)
        let teArr = Array.init si.LengthInTextElements (fun i -> si.SubstringByTextElements(i,1))
        Array.Reverse(teArr) //in-place reversal better performance than Array.rev
        String.Join("", teArr)

    let isPalindrome (x) = 
        x.ToString() = rev(x)

    let palindrome_numbers = seq {
        for i in 100 .. 999 do
            for j in 100 .. 999 do
                if isPalindrome(Convert.ToString(i*j)) then yield (i*j) }

module Eulers5 = 
    let isDivisableByNumbers arr x = 
        arr 
        |> List.exists (fun d -> x % d <> 0)
        |> not

    let smallestNumberDivisibleByAll dividers =
        let max = dividers |> List.max
        Seq.initInfinite ((+) 1 >> (*) max)
        |> Seq.find (isDivisableByNumbers dividers)
            
    let answer = smallestNumberDivisibleByAll [1..20]
                
            


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


        