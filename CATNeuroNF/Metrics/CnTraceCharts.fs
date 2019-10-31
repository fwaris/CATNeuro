namespace CATNeuro 
module CnTrace =
    open FSharp.Charting
    open System.Windows.Forms.DataVisualization
    open System.Windows.Forms
    open System.Drawing
    open System
    open CnMetrics
   

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

    let chLine title obs =
        let ch =
            LiveChart.FastLineIncremental(obs) 
            |> Chart.WithTitle title
            |> Chart.WithTitle(Color=System.Drawing.Color.DarkBlue, FontSize=8.0)
        ch


    let containerize ch = 
        let chh = new ChartTypes.ChartControl(ch,Dock=DockStyle.Fill)
        chh

    let container title chlist =
        let form = new Form()
        form.Width  <- 400
        form.Height <- 600
        form.Visible <- true 
        form.Text <- title //"CATNeuro Charts"
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

    let  obsNewGen = obsAll |> Observable.choose (function NewGen g -> Some g | _ -> None )

    let withObs fltr =
        obsAll
        |> Observable.filter fltr
        |> Observable.withI
        |> Observable.map fst
        |> Observable.together obsNewGen
        |> Observable.filter(function (Some _, Some _) -> true | _ -> false)
        |> Observable.map(fun (a,b)->a.Value, b.Value)

    let openCharts title popId =
        let obsNodeAdd = withObs (function NodeAdd p when p = popId -> true | _ -> false)
        let obsNodeMs = withObs (function NodeAddMiss p when p = popId  -> true | _ -> false)
        let obsCnnAdd = withObs (function ConnAdd p when p = popId -> true | _ -> false)
        let obsCnnAddMs = withObs (function ConnAddMiss p when p = popId  -> true | _ -> false)
        let obsXfer = withObs (function Xfer p when p = popId -> true | _ -> false)
        let obsXferM = withObs (function XferMiss p when p = popId  -> true | _ -> false)
        let obsCnnTggle = withObs (function CnnTggle p when p = popId -> true | _ -> false)
        let obsCnnTggleM = withObs (function CnnTggleMiss p when p = popId  -> true | _ -> false)
        let obsParm = withObs (function ParmMutate p when p = popId -> true | _ -> false)
        let obsNorms = withObs (function Norms (p,_) when p = popId  -> true | _ -> false)
        [
            chLine "Node Add" obsNodeAdd
            chLine "Node Add Miss" obsNodeMs
            chLine "Conn Add" obsCnnAdd
            chLine "Conn Add Miss" obsCnnAddMs
            chLine "Crossover" obsXfer
            chLine "Crossover Miss" obsXferM
            chLine "Conn Tggle" obsCnnTggle
            chLine "Conn Tggle Miss" obsCnnTggleM
            chLine "Parm Mutate" obsParm
        ]
        |> container title
