namespace Terje

module Sudoku =

    let easypuzzle = array2D [[0;0;4;1;3;0;0;0;2];
                          [1;9;0;6;0;2;0;0;0];
                          [3;0;0;0;4;0;9;0;0];
                          [8;0;0;0;6;0;0;9;4];
                          [2;0;3;9;0;1;6;0;5];
                          [9;7;0;0;2;0;0;0;3];
                          [0;0;8;0;9;0;0;0;1];
                          [0;0;0;8;0;6;0;2;7];
                          [4;0;0;0;5;3;8;0;0];]

    let mediumpuzzle = array2D [[0;0;6;2;0;0;0;3;7];
                                [0;0;9;0;0;0;0;2;0];
                                [7;0;8;0;9;0;0;6;0];
                                [6;0;5;1;2;0;0;0;0];
                                [0;4;0;0;5;0;0;9;0];
                                [0;0;0;0;4;7;6;0;1];
                                [0;3;0;0;1;0;2;0;6];
                                [0;6;0;0;0;0;5;0;0];
                                [5;7;0;0;0;9;3;0;0];]

    let evilpuzzle = array2D [[0;0;8;0;5;0;0;0;0];
                              [9;0;0;0;0;0;0;7;0];
                              [0;0;1;0;3;2;0;0;4];
                              [0;9;0;0;0;5;0;0;6];
                              [0;4;0;0;7;0;0;2;0];
                              [5;0;0;1;0;0;0;9;0];
                              [7;0;0;6;8;0;5;0;0];
                              [0;1;0;0;0;0;0;0;3];
                              [0;0;0;0;4;0;7;0;0];]

    let getCol index (matrix:_[,]) =
        matrix.[*,index]

    let getRow index (matrix:_[,]) =
        matrix.[index,*]

    let getSquare index (vars: 'a[,]) = 
        let x = index / 3
        let y = index % 3
        let startx, endx = x * 3, (x + 1) * 3 - 1
        let starty, endy = y * 3, (y + 1) * 3 - 1
        let flatten (matrix:'a[,]) = matrix |> Seq.cast<'a> |> Seq.toArray
        flatten vars.[startx .. endx, starty .. endy]

    let findPossibleNumbers (x:int, y:int, puzzle:_[,]) = 
        let findUsedNumbers (x:int[]) =
            x 
            |> Array.filter (fun x -> x > 0)
        let colNumbers = 
            (getCol x puzzle)
            |> findUsedNumbers
        let rowNumbers = 
            (getRow y puzzle)
            |> findUsedNumbers
        let squareNumbers = 
            let getSquareNumber (x, y) =
                (x/3) + ((y/3) * 3)
            getSquare (getSquareNumber(x,y)) puzzle
            |> findUsedNumbers
        let possibles() = 
            let usedNumbers = Array.append (Array.append rowNumbers colNumbers) squareNumbers
            set [1..9] - set usedNumbers
        possibles()

    type Pos = { X:int; Y:int }
    type SudokuVal = { Pos: Pos; Value: int }

    let rec solve (puzzle:_[,]) = 
        let findPossibles x y v = 
            if v > 0 then // already has value
                Set.empty
            else findPossibleNumbers(y, x, puzzle)
        let matrixOfPossibilities = 
            puzzle 
            |> Array2D.mapi findPossibles
        let findNumbersThatAppearsOnlyOnceInArrayOfPossibilities arrayOfArrays =
            let uniqueNumbers =
                arrayOfArrays 
                |> Seq.concat // concat all numbers in array
                |> Seq.groupBy (fun x -> x) // groupBy to find number of occurrence
                |> Seq.filter (fun x -> Seq.length (snd x) = 1) // filter out only numbers appearing once meaning its the only place this number could be
                |> Seq.map fst // we only need the key of the groupby result
            // return a touple of uniqueNumbers and their index in the arrayOfArrays
            let findIndexOfArray x = 
                arrayOfArrays |> Seq.findIndex (fun array -> Seq.exists (fun y -> y = x) array)
            uniqueNumbers |> Seq.map (fun x -> ((findIndexOfArray x), x))
        let checkColsForUniquePossibilities() = 
            [0..8] 
            |> Seq.map (fun x -> findNumbersThatAppearsOnlyOnceInArrayOfPossibilities (getCol x matrixOfPossibilities))
            |> Seq.mapi (fun i x -> (i, x)) // we need to know the index
            |> Seq.filter (fun x -> Seq.length (snd x) > 0) // filter out empty lists
            |> Seq.map (fun x -> (fst x, Seq.head (snd x))) // we dont need to keep the record in a seq
            |> Seq.map (fun x -> { Pos = { X = fst x; Y = fst (snd x) }; Value = snd (snd x) }) // finally set up the records as needed
        let checkRowsForUniquePossibilities() = 
            [0..8] 
            |> Seq.map (fun x -> findNumbersThatAppearsOnlyOnceInArrayOfPossibilities (getRow x matrixOfPossibilities))
            |> Seq.mapi (fun i x -> (i, x)) // we need to know the index
            |> Seq.filter (fun x -> Seq.length (snd x) > 0) // filter out empty lists
            |> Seq.map (fun x -> (fst x, Seq.head (snd x))) // we dont need to keep the record in a seq
            |> Seq.map (fun x -> { Pos = { Y = fst x; X = fst (snd x) }; Value = snd (snd x) }) // finally set up the records as needed
        let checkSquaresForUniquePossibilities() = 
            let getCoords (squareIndex, arrayIndex) = 
                let x = (squareIndex % 3 * 3) + (arrayIndex % 3)
                let y = (squareIndex / 3 * 3) + (arrayIndex / 3)
                (x, y)
            let getFinalRecord x = 
                let squareIndex = fst x
                let arrayIndex = fst (snd x)
                let value = snd (snd x)
                let coords = getCoords (squareIndex, arrayIndex)
                let record = { Pos = { Y = snd coords; X = fst coords }; Value = value }
                record
            [0..8] 
            |> Seq.map (fun x -> findNumbersThatAppearsOnlyOnceInArrayOfPossibilities (getSquare x matrixOfPossibilities))
            |> Seq.mapi (fun i x -> (i, x)) // we need to know the index
            |> Seq.filter (fun x -> Seq.length (snd x) > 0) // filter out empty lists
            |> Seq.map (fun x -> (fst x, Seq.head (snd x))) // we dont need to keep the record in a seq
            |> Seq.map getFinalRecord // finally set up the records as needed
        let findValues () = 
            checkColsForUniquePossibilities()
            |> Seq.append (checkRowsForUniquePossibilities())
            |> Seq.append (checkSquaresForUniquePossibilities())
        let setField s = 
            puzzle.[s.Pos.Y, s.Pos.X] <- s.Value
        let isSolved p =
            let flatten (matrix:'a[,]) = matrix |> Seq.cast<'a> |> Seq.toArray
            let array = flatten p
            if Seq.exists (fun x -> x = 0) array then
                false
            else true
        let newValues = findValues()
        if Seq.length newValues = 0 then
            printfn "Could not find a solution"
            printfn "%A" puzzle
        else
            newValues 
            |> Seq.iter setField
            if isSolved puzzle then
                printfn "%A" puzzle
            else 
                solve puzzle
                printfn "iter"

        

    

    

