
#r @"\\psf\Home\Desktop\GitHub\eulersfsharp\src\Euler\bin\Debug\FSharp.Data.dll"
#r @"\\psf\Home\Desktop\GitHub\eulersfsharp\src\packages\MSDN.FSharpChart.dll.0.60\lib\MSDN.FSharpChart.dll"
#r "System.Windows.Forms.DataVisualization.dll"

open FSharp.Data
open MSDN.FSharp.Charting
open System.Windows.Forms
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

module MovieCounter = 
    
    // using fsharp type providers to parse the csv data which actually gives us strongly typed access to the csv data
    type Movie = CsvProvider<"http://files.figshare.com/1332945/film_death_counts.csv">
    let movies = Movie.Load("http://files.figshare.com/1332945/film_death_counts.csv").Data

    let top25MostViolentMovies = 
        movies 
        |> Seq.sortBy (fun x -> -((float)x.Body_Count / (float)x.Length_Minutes)) 
        |> Seq.take 25

    let chartData = 
        top25MostViolentMovies
        |> List.ofSeq
        |> List.map (fun x -> x.Film + " (" + x.Year.ToString() + ")", float x.Body_Count / float x.Length_Minutes)
        |> List.rev
        
    // chart formatting
    let chart = 
        chartData
        |> FSharpChart.Bar
        |> FSharpChart.WithArea.AxisY(Minimum = 2.0, Maximum = 5.5)
        |> FSharpChart.WithArea.AxisX(LabelStyle = new LabelStyle(Interval = 1.0))

    // draw the chart
    let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 700)
    let ctl = new ChartControl(chart, Dock = DockStyle.Fill)
    form.Controls.Add(ctl)
