module CnTrace

open FSharp.Charting
open System.Windows.Forms.DataVisualization
open System.Windows.Forms
open System.Drawing
open System
let chGrid = ChartTypes.Grid(Enabled=false,Interval=0.1)
let ls = ChartTypes.LabelStyle(TruncatedLabels=true, Interval=0.2, Format="{0:F1}")

let round' x = System.Math.Round((x:float),2)

let chPtsLine title obs =
    let obs1 = obs
    let ch =
        [
          LiveChart.FastPoint(obs1) 
          |> Chart.WithSeries.Marker(Size=10, Color=Color.Transparent, BorderColor=Color.DarkBlue, BorderWidth=2)
        ]
        |> Chart.Combine 
        |> Chart.WithTitle title
        |> Chart.WithTitle(Color=System.Drawing.Color.DarkBlue)
        |> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid, LabelStyle=ls)
        |> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid)
    ch


let containerize ch = 
    let chh = new ChartTypes.ChartControl(ch,Dock=DockStyle.Fill)
    chh

let container chlist =
    let form = new Form()
    form.Width  <- 400
    form.Height <- 600
    form.Visible <- true 
    form.Text <- "CATNeuro Charts"
    let grid = new TableLayoutPanel()
    grid.AutoSize <- true
    grid.ColumnCount <- 3
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33.f)) |> ignore
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33.f)) |> ignore
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33.f)) |> ignore
    grid.RowCount <- 2
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,50.f)) |> ignore
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,50.f)) |> ignore
    grid.GrowStyle <-  TableLayoutPanelGrowStyle.AddRows
    grid.Dock <- DockStyle.Fill
    let containers = chlist |> List.map containerize
    containers |> List.iter grid.Controls.Add
    form.Controls.Add(grid)
    form.Show()
    form

let openCharts() =
    let obs = 
        Metrics.obsAll 
        |> Observable.choose (function Metrics.NodeAdd _ -> Some(1) | _ ->None)
        |> Observable.withI
        |> Observable.map (fun (i,_) -> [i,i])

    [
        chPtsLine "NodesAdd" obs
    ]
