// Learn more about F# at http://fsharp.net

// Eulers 1

// create the list of nubers from 1 to 1000
let numbers = [1 .. 999];

// get numbers moddable with 3 or 5
let r1 = List.filter (fun x -> x % 3 = 0 || x % 5 = 0) numbers

// sum up numbers
let r2 = List.sum r1

// print result
printfn "sum: %A" r2