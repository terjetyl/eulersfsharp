#r "bin/Debug/Quandl.NET.dll"

open Quandl.NET.QuandlDotNet
open System.Collections.Generic

let quandl = new Quandl()
let settings = new Dictionary<string,string>()
settings.Add("start_date", "1945-12-31")
settings.Add("end_date", "2013-12-31")

type TcpRow = { Individual_Income_Taxes: string; Corporation_Income_Taxes: string; Fiscal_Year:string }

let data = quandl.GetData<TcpRow>("TPC/HIST_RECEIPT", settings, "csv")