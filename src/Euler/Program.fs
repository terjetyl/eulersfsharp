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

    let isPrime (possiblePrime:int64) =
        let sqrRootOfPrime = int64(System.Math.Sqrt(float(possiblePrime)))
             
        {1L .. sqrRootOfPrime}
        |> Seq.forall(fun divisor -> 
            match divisor with
            | 1L -> true
            | x when divisor = possiblePrime -> true
            | _ -> possiblePrime % divisor > 0L)

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
    let square x = x * x
    let answer = ([1..100] |> Seq.sum |> square) - ([1..100] |> List.map square |> Seq.sum)

    printfn "Eulers 6 : %A" answer

module Eulers7 =
    let answer = 
        Seq.initInfinite (fun n -> int64 n + 1L) 
        |> Seq.filter Primes.isPrime 
        |> Seq.take 10002 
        |> Seq.max

module Eulers8 = 
    let numbers = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

    let parseInt x =  int (Char.GetNumericValue(x))

    let ints = Seq.map parseInt numbers

    // put in groups of 5 and 5
    let factors = Seq.windowed 5 ints

    let sums x = Seq.reduce (*) x

    let answer = Seq.max (Seq.map sums factors)

module Eulers9 =
    let tripletsSum1000 =
        seq {
            for a in 1 .. 333 do
                for b in a + 1 .. 499 do
                    let c = 1000 - b - a
                    if a < b && b < c then
                         yield (a,b,c)
         }

    let answer =
         tripletsSum1000
         |> Seq.find (fun (a,b,c) -> a * a + b * b = c * c)
         |> fun (a,b,c) -> a*b*c

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


        