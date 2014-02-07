#r "../packages/HtmlAgilityPack.1.4.6/lib/Net45/HtmlAgilityPack.dll"
#r "../packages/FSharp.Data.1.1.10/lib/net40/FSharp.Data.dll"

#load "HtmlAgilityPack.FSharp.fs"

open System
open System.Linq
open System.Net
open System.Text.RegularExpressions
open FSharp.Net
open System.IO
open FSharp.Data

open HtmlAgilityPack
open HtmlAgilityPack.FSharp

let loadHtml (url) = 
    try
        url 
        |> Http.AsyncRequest 
        |> Async.RunSynchronously
    with
        | ex -> 
            printfn "%s failed: %s" url ex.Message
            String.Empty

let toNumeric text = 
    decimal (Regex.Replace(text, @"[^\d]", ""))

let getOnlyNumbers text = 
    Regex.Replace(text, @"[^\d]", "")

type Movie = { Name : string; Year : string; Kills : string; ImdbUrl : string }
type imdbMovie = { Film : string; Year : int; LengthInMinutes : int; Kills: string; MPAARating : string; Genre : string; Directors : string list; ImdbRating : decimal; Cast : string list }

let parseImdbMovie (movie:Movie) = 
    async{
        printfn "parsing %s async" movie.Name
        let id = getOnlyNumbers(movie.ImdbUrl)
        let html = loadHtml ("http://www.imdb.com/title/tt" + id)
        let doc = html |> createDoc
        let film = doc.SelectSingleNode("//h1[@class='header']/span[@class='itemprop']").InnerText
        let year = doc.SelectSingleNode("//h1[@class='header']/span[@class='nobr']").InnerText |> toNumeric
        let lengthMinutes = 
            let nodes = doc.SelectNodes("//div[@class='infobar']/time[@itemprop='duration']")
            if nodes.Count > 0 then
                nodes.First().InnerText |> toNumeric
            else 0m
        let mpaaRating = 
            let nodes = doc.SelectNodes("//div[@class='infobar']/span/@content")
            if nodes = null then 
                (@"UNRATED")
            else nodes.First().InnerText
        let genre = doc.SelectSingleNode("//span[@class='itemprop' and @itemprop='genre']").InnerText
        let directors = doc.SelectNodes("//div[@itemprop='director']/a") |> Seq.map (fun x -> x.InnerText) |> Seq.toList
        let imdbRating = decimal (doc.SelectSingleNode("//div[@class='titlePageSprite star-box-giga-star']").InnerText)
        let creditsHtml = loadHtml ("http://www.imdb.com/title/tt" + id + "/fullcredits")
        let castDoc = creditsHtml |> createDoc
        let cast = castDoc.SelectNodes("//span[@itemprop='name']") |> Seq.map (fun x -> x.InnerText) |> Seq.toList
        let imdbInfo = { Film = film; Year = int year; LengthInMinutes = int lengthMinutes; MPAARating = mpaaRating; Genre = genre; Directors = directors; ImdbRating = imdbRating; Cast = cast; Kills = movie.Kills }
        return imdbInfo
    }

type imdbFile = CsvProvider<"../Files/test.csv">
let movies = imdbFile.Load("../Files/test.csv").Data

let getExtendedMovieInfo() = 
    let tmp = movies |> Seq.map (fun x -> { Name = x.Film; Year = x.Year.ToString(); Kills = x.Body_Count.ToString(); ImdbUrl = x.IMDB_URL }) 
    Async.Parallel [for m in tmp -> parseImdbMovie(m)]
    |> Async.RunSynchronously

let writeToFile() = 
    let outFile = new StreamWriter("c:\\tmp\\film-death-counts-FSharp.csv")
    outFile.WriteLine("Film,Year,Body_Count,MPAA_Rating,Genre,Director,Actors,Length_Minutes,IMDB_Rating")
    printfn "Starting at: %s" (DateTime.Now.ToString())
    let movies = getExtendedMovieInfo()
    movies
    |> Seq.iter (fun l -> 
                    outFile.WriteLine(sprintf "%s, %s, %s, %s, %s, %s, %s, %s, %s" l.Film (string l.Year) l.Kills l.MPAARating l.Genre (String.Join("|", l.Directors)) (String.Join("|", l.Cast)) (string l.LengthInMinutes) (string l.ImdbRating))
                    Console.WriteLine l.Film)
    outFile.Flush()
    outFile.Close()
    printfn "Finished at: %s" (DateTime.Now.ToString())