#r @"bin\Debug\FSharp.Data.dll"
#r @"..\packages\RavenDB.Client.2.5.2750\lib\net45\Raven.Client.Lightweight.dll"
#r @"..\packages\RavenDB.Client.2.5.2750\lib\net45\Raven.Abstractions.dll"

open System
open FSharp.Data
open Raven.Client;

let db = 
    let store = (new Raven.Client.Document.DocumentStore(Url = "http://localhost:8080", DefaultDatabase = "Stocks" )).Initialize()
    use session = store.OpenSession()
    session

//[<CLIMutable>]
type Transaction = { Ticker : string; Volume : int; Price : decimal ; TransactionType : char; Date : DateTime }

type Stock = { Ticker : string; Name : string; Buy : decimal; Sell : decimal; Last : decimal; Volume : int; LastUpdate : DateTime; Change : decimal; High : decimal; Low : decimal; Open : decimal; Prev : decimal; BidDepthTotal : int; OfferDepthTotal : int }

type Asset = { Ticker : string; Volume : int; Average : decimal; Cost : decimal; MarketValue : decimal; Realized : decimal; NotRealized : decimal }

type Portfolio = { Assets : Asset list }

type StockService = CsvProvider<"http://norma.netfonds.no/kurs.php?layout=horizontal&exchange=OSE&sec_types=&ticks=&table=tab&sort=alphabetic", "\t">
let quotes = StockService.Load("http://norma.netfonds.no/kurs.php?layout=horizontal&exchange=OSE&sec_types=&ticks=&table=tab&sort=alphabetic").Data

let currentQuotes = 
    quotes 
    |> Seq.map (fun row -> { 
                            Ticker = row.paper; 
                            Name = row.name; 
                            Last = row.last; 
                            Buy = row.bid; 
                            Sell = row.offer; 
                            Change = row.change; 
                            High = row.high; 
                            Low = row.low; 
                            Open = row.``open``; 
                            Volume = row.volume; 
                            Prev = row.prev_price; 
                            LastUpdate = DateTime.Now; 
                            BidDepthTotal = row.bid_depth_total; 
                            OfferDepthTotal = row.offer_depth_total })
    |> Seq.toList

let transactions = 
    [{ Ticker = "SSI"; Volume = 96; Price = 74.75m; TransactionType = 'B'; Date = new DateTime(2014, 1, 28) };
     { Ticker = "AGR"; Volume = 5182; Price = 4.80m; TransactionType = 'B'; Date = new DateTime(2014, 1, 28) };
     { Ticker = "PDR"; Volume = 3000; Price = 8.24m; TransactionType = 'B'; Date = new DateTime(2014, 1, 28) }; 
     { Ticker = "IMSK"; Volume = 2110; Price = 11.75m; TransactionType = 'B'; Date = new DateTime(2014, 1, 28) };
     { Ticker = "BERGEN"; Volume = 5000; Price = 3.35m; TransactionType = 'B'; Date = new DateTime(2014, 1, 28) };
     { Ticker = "BERGEN"; Volume = 100; Price = 3.60m; TransactionType = 'B'; Date = new DateTime(2014, 1, 28) }]

let getTransactions() = 
    db.Query<Transaction>()

let getQuote (ticker:string, quotes:Stock list) = 
        let quote = 
            quotes 
            |> Seq.find (fun x -> x.Ticker = ticker)
        quote

let calculatePortfolio (transactions:Transaction list, quotes: Stock list) = 
    let assets = 
        transactions 
        |> Seq.map (fun t -> 
                        let quote = getQuote (t.Ticker, quotes)
                        let marketValue = (decimal)t.Volume*quote.Last
                        { 
                            Ticker = t.Ticker; 
                            Volume = t.Volume;
                            MarketValue = marketValue; 
                            Cost = decimal t.Volume*t.Price;
                            Realized = 0.0m; 
                            NotRealized = decimal t.Volume*quote.Last - decimal t.Volume*t.Price; 
                            Average = t.Price 
                        }
                   )
    assets


let buy volume ticker = 
    let quote = getQuote (ticker, currentQuotes)
    let total = quote.Sell * (decimal)volume
    printfn "You bought %d %s stocks for a total of %f" volume ticker total
    let transaction = { Ticker = ticker; Volume = volume; Price = quote.Sell; TransactionType = 'B'; Date = DateTime.Now }
    db.Store(transaction)
    db.SaveChanges()
    transactions = (transaction::transactions)

let sell volume ticker = 
    let quote = getQuote (ticker, currentQuotes)
    let total = quote.Buy * (decimal)volume
    printfn "You sold %d %s stocks for a total of %f" volume ticker total
    let transaction = { Ticker = ticker; Volume = volume; Price = quote.Sell; TransactionType = 'S'; Date = DateTime.Now }
    db.Store(transaction)
    db.SaveChanges()

let showportfolio() = 
    let ps = calculatePortfolio(transactions, currentQuotes)
    for p in ps do
        printfn "%s %d %f" p.Ticker p.Volume p.MarketValue