open System
open System.Linq
open System.Net
open System.Text.RegularExpressions

let lettersAndNumbers = ['A'..'Z'] |> List.map (fun x -> match x with | 'X' -> 'x' | 'V' -> 'v' | _ -> x) |> List.append ['0'..'9']

let urls = lettersAndNumbers |> List.map (fun x -> "http://www.moviebodycounts.com/movies-" + string x + ".htm")

let scrapeWebpage url = 
    async {
        use wc = new WebClient()
        let! html = wc.AsyncDownloadString(Uri(url))
        let matchCollection = Regex.Matches(html, @"(<a.*?>.*?</a>)")
        return matchCollection |> Seq.cast |> List.iter (fun (x:Match) -> printfn "%A" x)
    }
    |> Async.RunSynchronously
    |> printfn "%s"