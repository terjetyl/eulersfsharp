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

let lettersAndNumbers = 
    {'A'..'Z'}
    |> Seq.map (fun x -> match x with | 'X' -> 'x' | 'V' -> 'v' | _ -> x) 
    |> Seq.append {'0'..'9'}

let urls = 
    lettersAndNumbers 
    |> Seq.map (fun x -> "http://www.moviebodycounts.com/movies-" + string x + ".htm")

let getLink htmlLink = 
    Regex.Match(htmlLink, @"href=""(.*?)""").Groups.[1].Value

let addBaseUrl (partial:string) =
    if partial.IndexOf "http://www.moviebodycounts.com/" > 1 then
        partial
    else
        "http://www.moviebodycounts.com/" + partial

let scrapeLink url = 
    async {
        try
            use wc = new WebClient()
            let! html = wc.AsyncDownloadString(Uri(url))
            let matchCollection = Regex.Matches(html, @"(<a.*?>.*?</a>)")
            return matchCollection 
                |> Seq.cast 
                |> Seq.map (fun (c:Match) -> c.Groups.[1].Value)
        with error -> 
            printfn "failed to get %s, %A" url error 
            return Seq.empty
    }
    |> Async.RunSynchronously

let scrapeAllMovieLinks () =
    urls 
    |> Seq.map scrapeLink // get links to all movies pages
    |> Seq.concat // put all links in one array
    |> Seq.map (fun x -> getLink x) // parse out only the href part
    |> Seq.filter (fun x -> x <> @"contact.htm") // remove contact pages
    |> Seq.map (fun x -> addBaseUrl x) // add base url if it does not exist
    |> Seq.toList

let loadHtml (url) = 
    url |> Http.AsyncRequest |> Async.RunSynchronously

let getNode html xpath = 
    let doc = html |> createDoc
    doc.SelectNodes(xpath).First()

let getTitle html = 
    let node = getNode html "//title"
    node.InnerText.Replace("Movie Body Counts: ", "")

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

// define a movie object as a record type
type Movie = { Name : string; Year : string; Kills : string; ImdbUrl : string }

let parseMovie html = 
    let title = getTitle html
    let kills = getKills html
    let year = getYear html
    let imdb = getImdbUrl html

    let movie = { Name = title; Year = year; Kills = kills; ImdbUrl = imdb }
    movie

let parseMovies() = 
    scrapeAllMovieLinks()
    |> Seq.map loadHtml
    |> Seq.map parseMovie
    
let writeMoviesToFile() = 
    let movies = parseMovies()
    let outFile = new StreamWriter("c:\\tmp\\Test.csv")
    outFile.WriteLine("IMDB_URL, Film, Year, Body_Count")
    movies 
    |> Seq.iter (fun l -> outFile.WriteLine(sprintf "%s, %s, %s, %s" l.ImdbUrl l.Name l.Year l.Kills))
    outFile.Flush()
    outFile.Close()