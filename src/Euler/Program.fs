// Learn more about F# at http://fsharp.net

// Eulers 1

// create the list of nubers from 1 to 1000
let numbers = [1 .. 999];

// get numbers moddable with 3 or 5
let r1 = List.filter (fun x -> x % 3 = 0 || x % 5 = 0) numbers

// sum up numbers
let answer1 = List.sum r1

// print answer
printfn "sum: %A" answer1


// Eulers 2

// calculates the value of the item in the Fibonacci seq at the given position
let rec Fib(n) = 
    if (n < 2) then
        1
    else
        Fib(n-2) + Fib(n-1)

let FibSeq = Seq.unfold (fun (a, b) -> Some( a, (b, a + b) ) ) (1, 2)

let answer2 = 
    FibSeq
        |> Seq.filter (fun x -> x % 2 = 0)
        |> Seq.takeWhile (fun x -> x <= 4000000)
        |> Seq.sum


// Eulers 6
let numbers6 = [1..100];

let square = fun x -> x * x

// list with the square of the first 100 numbers
let list = List.map square numbers6

let sumofsquares = List.sum list

let summednumbers = List.sum numbers6

let answer = square(summednumbers) - sumofsquares

printfn "%A" answer
