open System

module SlotMachine = 

    type slotType = 
        | Grape
        | Apple
        | WildCherry
        | Bell
        | Bar
        | Lucky7

    let slotCol = [slotType.Grape;slotType.Grape;slotType.Grape;slotType.Grape;slotType.Grape;slotType.Grape;slotType.Grape;slotType.Grape;slotType.Apple;slotType.Apple;slotType.Apple;slotType.Apple;slotType.Apple;slotType.Apple;slotType.WildCherry;slotType.WildCherry;slotType.WildCherry;slotType.WildCherry;slotType.WildCherry;slotType.Bell;slotType.Bell;slotType.Bell;slotType.Bell;slotType.Bar;slotType.Bar;slotType.Bar;slotType.Lucky7;slotType.Lucky7]

    let shuffle list = 
        let rand = new System.Random()
        list 
            |> List.map (fun c -> (rand.Next(), c))
            |> List.sortBy fst
            |> List.map snd

    let pickRandom() = 
        let rand = new System.Random()
        let shuffledList = shuffle slotCol
        shuffledList.[rand.Next(0,shuffledList.Length)]

    let startGame() = 
        let rand = new System.Random()
        let shuffledList = slotCol |> shuffle
        let a = shuffledList.[rand.Next(0, slotCol.Length)]
        let b = shuffledList.[rand.Next(0, slotCol.Length)]
        let c = shuffledList.[rand.Next(0, slotCol.Length)]
        let result = (a, b, c)
        match result with
            | (slotType.Grape, slotType.Grape, slotType.Grape) -> 3
            | (slotType.Apple, slotType.Apple, slotType.Apple) -> 6
            | (slotType.WildCherry, slotType.WildCherry, slotType.WildCherry) -> 10
            | (slotType.Bell, slotType.Bell, slotType.Bell) -> 20
            | (slotType.Bar, slotType.Bar, slotType.Bar) -> 40
            | (slotType.Lucky7, slotType.Lucky7, slotType.Lucky7) -> 100
            | (_,_,_) -> -1

    let test() = seq {
        for i in [1..1000] do
            yield startGame()
    }

    let stats() = 
        test()
        |> Seq.groupBy (fun x -> x)
