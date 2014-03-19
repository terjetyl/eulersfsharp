namespace Tests

module MovieCounter = 

    open FSharp.Data
    open MSDN.FSharp.Charting
    
    type Movie = CsvProvider<"http://files.figshare.com/1332945/film_death_counts.csv">
    let movies = Movie.Load("http://files.figshare.com/1332945/film_death_counts.csv")

    let top25MostViolentMovies = 
        movies.Data 
        |> Seq.sortBy (fun x -> -(x.Body_Count / x.Length_Minutes)) 
        |> Seq.take 25

//    [<EntryPoint>]
//    let main argv = 
//        top25MostViolentMovies 
//            |> Seq.map (fun x -> x.Film + " (" + x.Year.ToString() + ")", x.Body_Count / x.Length_Minutes)
//            |> FSharpChart.Bar
//        0 // return an integer exit code