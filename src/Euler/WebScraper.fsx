#r "../packages/HtmlAgilityPack.1.4.6/lib/Net45/HtmlAgilityPack.dll"
#r "../packages/FSharp.Data.1.1.10/lib/net40/FSharp.Data.dll"

#load "HtmlAgilityPack.FSharp.fs"

open System
open System.Linq
open System.Net
open System.Text.RegularExpressions
open FSharp.Net
open System.IO

open HtmlAgilityPack
open HtmlAgilityPack.FSharp

let getUrlToAllIndexPages() = 
    let lettersAndNumbers = 
        {'R'..'Z'}
        |> Seq.append {'A'..'P'}
        |> Seq.map (fun x -> match x with | 'X' -> 'x' | 'V' -> 'v' | _ -> x) // lowercase only x and v
    let urls = 
        lettersAndNumbers 
        |> Seq.map (fun x -> "http://www.moviebodycounts.com/movies-" + string x + ".htm")
        |> Seq.append [ "http://www.moviebodycounts.com/movies-numbers.htm" ]
    urls

let loadHtml (url) = 
    try
        url 
        |> Http.AsyncRequest 
        |> Async.RunSynchronously
    with
        | ex -> 
            printfn "%s failed: %s" url ex.Message
            String.Empty

let loadHtmlAsync (url) = 
    printfn "parsing %s" url
    url |> Http.AsyncRequest

let addBaseUrl (partial:string) =
    if partial.IndexOf "http://www.moviebodycounts.com/" > -1 then
        partial
    else
        "http://www.moviebodycounts.com/" + partial

let findMovieLinks html = 
    let doc = html |> createDoc
    let links = doc.SelectNodes("//img[@src='graphic-movies.jpg']/following::a/@href")
    links 
    |> Seq.map (fun x -> x.Attributes.["href"].Value)
    |> Seq.map (fun x -> x.Replace("&amp;", "%26"))

let findAllMovieLinks () =
    getUrlToAllIndexPages() 
    |> Seq.map loadHtmlAsync
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.map findMovieLinks // get links to all movies pages
    |> Seq.concat // put all links into one array
    |> Seq.filter (fun x -> x <> "movies-C.htm") // remove invalid link
    |> Seq.map (fun x -> addBaseUrl x) // add base url if it does not exist
    |> Seq.toList

// define a movie object as a record type
type Movie = { Name : string; Year : string; Kills : string; ImdbUrl : string }

let parseMovie html = 
    let getNode html xpath = 
        let doc = html |> createDoc
        doc.SelectNodes(xpath).First()
    let getTitle html = 
        let node = getNode html "//title"
        node.InnerText.Replace(System.Environment.NewLine, "").Replace("Movie Body Counts: ", "")
    let getKills html = 
        let node = getNode html "//img[@src='graphic-bc.jpg']/following::text()[normalize-space()]"
        let a = Regex.Replace(node.InnerText, "\\(.*?\\)", "")
        Regex.Replace(node.InnerText, "[^0-9]+", "")
    let getYear html = 
        let node = getNode html "//a[contains(@href, 'charts-year')]"
        node.InnerText
    let getImdbUrl html = 
        let node = getNode html "//a/@href[contains(.,'imdb')]"
        node.Attributes.["href"].Value
    let title = getTitle html
    printfn "parsing %s" title
    let kills = getKills html
    let year = getYear html
    let imdb = getImdbUrl html
    let movie = { Name = title; Year = year; Kills = kills; ImdbUrl = imdb }
    movie

let parseMovies() = 
    findAllMovieLinks()
    |> Seq.map loadHtmlAsync
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.map parseMovie
    
let writeMoviesToFile() = 
    printfn "Starting at: %s" (DateTime.Now.ToString())
    let movies = parseMovies()
    let outFile = new StreamWriter("c:\\tmp\\Test.csv")
    outFile.WriteLine("IMDB_URL, Film, Year, Body_Count")
    movies 
    |> Seq.iter (fun l -> outFile.WriteLine(sprintf "%s, %s, %s, %s" l.ImdbUrl l.Name l.Year l.Kills))
    outFile.Flush()
    outFile.Close()
    printfn "Finished at: %s" (DateTime.Now.ToString())