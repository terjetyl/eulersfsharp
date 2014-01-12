namespace Terje

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

    let getUsedNumbers (x:int[]) =
        x 
        |> Array.filter (fun x -> x > 0)

    let flatten (matrix:'a[,]) = matrix |> Seq.cast<'a> |> Seq.toArray

    let getCol c (matrix:_[,]) =
        flatten matrix.[*,c..c] 
 
    let getRow r (matrix:_[,]) =
        flatten matrix.[r..r,*]

    let getSquare s (vars: 'a[,]) = 
        let x = s / 3
        let y = s % 3
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

        let squareNumbers = 
            getSquare (getSquareNumber(x,y)) sudokuPuzzle
            |> getUsedNumbers

        let findPossibles() = 
            let usedNumbers = Array.append (Array.append rowNumbers colNumbers) squareNumbers
            System.Linq.Enumerable.Except([1..9], usedNumbers)
            |> Seq.toArray

        findPossibles()

    let augmentWithPossibles x y v = 
        if v > 0 then
            (v, Array.empty)
        else (v, getPossibles(y, x))

    let checkBoard() = 
        let tmp = 
            sudokuPuzzle 
            |> Array2D.mapi augmentWithPossibles

        for x in 0..8 do
            for y in 0..8 do
                if Array.length (snd tmp.[x,y]) = 1 then 
                    printfn "%d, %d has a unique possibility %A" x y tmp

        let findIndexOfArrayThatHasNum (x:int, arrayWithArrays) = 
            arrayWithArrays
            |> Seq.findIndex (fun x -> x |> Seq.exists (fun i -> i = x))

        let findUniquePossibiltiesInCols() = 
            for x in 0..8 do
                let uniques = 
                    tmp.[*,x] 
                    |> Seq.map snd 
                let uniqueNum = 
                    uniques
                    |> Seq.concat
                    |> Seq.groupBy (fun x -> x) 
                    |> Seq.filter (fun x -> Seq.length (snd x) = 1)
                if Seq.length uniqueNum > 0 then
                    printfn "Found unique num for pos %d,%A %A" x (findIndexOfArrayThatHasNum((Seq.head uniqueNum), uniques)) uniqueNum
            printfn "Done"

        findUniquePossibiltiesInCols()

        
        

    

    

