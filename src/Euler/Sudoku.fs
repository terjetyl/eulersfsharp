namespace Terje

open Microsoft.FSharp
open MathNet.Numerics.LinearAlgebra.Complex

module Sudoku =
    // Input puzzle
    let sudokuPuzzle = array2D [[0;0;0;0;6;0;4;0;0];
                                [0;5;0;0;0;3;6;0;0];
                                [1;0;0;0;0;5;0;0;0];
                                [0;4;1;0;0;0;0;0;0];
                                [0;9;0;0;0;0;0;2;0];
                                [5;0;2;0;0;0;3;4;0];
                                [3;0;0;7;0;0;0;0;0];
                                [0;0;6;5;0;0;0;9;0];
                                [0;0;4;0;1;0;0;0;0];]

    let m = new DenseMatrix(3,3)

    let hasNineNumbers (x:int[]) =
        x 
        |> Array.filter (fun x -> x > 0)
        |> Array.length = 9

    let getUsedNumbers (x:int[]) =
        x 
        |> Array.filter (fun x -> x > 0)


    let flatten (matrix:'a[,]) = matrix |> Seq.cast<'a> |> Seq.toArray

    let getCol c (matrix:_[,]) =
        flatten matrix.[*,c..c] 
 
    let getRow r (matrix:_[,]) =
        flatten matrix.[r..r,*]

    let getSquare s (vars: 'a[,]) = 
        let x = s % 3
        let y = s / 3
        let startx, endx = x * 3, (x + 1) * 3 - 1
        let starty, endy = y * 3, (y + 1) * 3 - 1
        flatten vars.[startx .. endx, starty .. endy]

    let getPossibles (x:int, y:int) = 
        let colNumbers = 
            (getCol x sudokuPuzzle)
            |> getUsedNumbers

        let rowNumbers = 
            (getRow y sudokuPuzzle)
            |> getUsedNumbers

        let getSquareNumber (x, y) =
            (x/3) + ((y/3) * 3)

        let squareNumbers() = 
            getSquare (getSquareNumber(x,y)) sudokuPuzzle

        let findPossibles = 
             System.Linq.Enumerable.Except([1..9], Array.append rowNumbers colNumbers)
             |> Seq.toArray

        findPossibles

    
    let checkBoard() = 
        for x in 0..8 do
            for y in 0..8 do
                let possibles = (x, y, getPossibles(x, y))
                if Seq.length possibles = 1 then
                    printfn "%d,%d can only have %d" x y (Seq.head possibles)
                else printfn "%d, %d, %A" x y possibles
        
        

    

    

