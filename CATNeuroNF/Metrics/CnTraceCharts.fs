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

    open MathNet.Numerics.Statistics

    let dns bndwth xs data =
        let ds = xs |> Array.map (fun x -> KernelDensity.EstimateGaussian(x,bndwth,data))
        ds

    let estimateB data =
        let h = CATProb.stddev data * ((4./3./float data.Length) ** (1./5.))     //silverman's rule of thumb for bandwidth
        let mn = data |> Array.min
        let mx = data |> Array.max
        h,[| mn .. 0.1 .. mx |]

    let collectParms (obs:IObservable<Parm[]>) =
        let mutable d1 = Map.empty
        let mutable d2 = Map.empty
        { new IObservable<Map<_,_> * Map<_,_>> with
            member x.Subscribe(observer) = //TODO: need to dispose both 
                obs.Subscribe(fun parms -> 

                    try 
                        d1 <- (d1,parms) ||> Array.fold (fun acc p -> 
                            match p with 
                            | MDensity densty   -> let b,xs = estimateB densty.Density
                                                   let b = max b 0.1
                                                   //let rng = abs ((Seq.max densty.Density) - (Seq.min densty.Density))
                                                   //let b = if b <= 0. then (rng / 10.) else b
                                                   let ds = dns b xs densty.Density
                                                   acc  |> Map.add (densty.Innov,densty.Parm) {|S=densty.Density.Length; Xs=xs; Ys=ds |}
                            | _                 -> acc
                            )
                        d2 <- (d2,parms) ||> Array.fold (fun acc p -> 
                            match p with 
                            | MCat cat -> acc  |> Map.add (cat.Innov,cat.Parm) {|S=cat.Samples; Ctgs=cat.Categories|}
                            | _        -> acc
                            )
                    with ex ->
                        printfn "%A" ex.Message

                    observer.OnNext (d1,d2)
                    
                    )
        }

    let createNormsForm title  =
        let form = new Form()
        form.Width  <- 600
        form.Height <- 300
        form.Visible <- true 
        form.Text <- title //"CATNeuro Charts"
        let grid = new TableLayoutPanel()
        grid.AutoSize <- true
        grid.AutoScroll <- true
        grid.ColumnCount <- 3
        grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33.f)) |> ignore
        grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33.f)) |> ignore
        grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33.f)) |> ignore
        //grid.RowCount <- 2
        //grid.RowStyles.Add(new RowStyle(SizeType.Percent,50.f)) |> ignore
        //grid.RowStyles.Add(new RowStyle(SizeType.Percent,50.f)) |> ignore
        grid.GrowStyle <-  TableLayoutPanelGrowStyle.AddRows
        grid.Dock <- DockStyle.Fill
        form.Controls.Add(grid)
        form.Show()
        grid,form

    let openNormsChart uiCtx title popId =

        let obsNorms = 
            obsAll
            |> Observable.choose (function Norms (p,ns) when p = popId -> Some(ns) | _ -> None)
            |> collectParms


        let grid,form = createNormsForm title

        let disp =
            obsNorms 
            |> Observable.subscribe(fun (dnstyMap,catMap) ->
                let i = 1
                async {
                    do! Async.SwitchToContext uiCtx
                    form.SuspendLayout()
                    grid.Visible <- false
                    grid.Controls.Clear()
                    try 
                    
                        dnstyMap |> Map.iter (fun ((i,p) as k) ds ->

                            let newCh() =
                                Chart.Area( (ds.Xs,ds.Ys) ||> Array.zip) 
                                |> Chart.WithTitle (sprintf "%d %s [%d]" i p ds.S, FontSize=9.0)
                                |> containerize
                            let ch = newCh()
                            grid.Controls.Add(ch)

                        )

                        catMap |> Map.iter (fun ((i,p) as k) cat ->

                            let newCh() =
                                Chart.Bar cat.Ctgs
                                |> Chart.WithTitle (sprintf "%d %s[%d]" i p cat.S, FontSize=9.0)
                                |> containerize                        
                            let ch = newCh()
                            grid.Controls.Add(ch)

                        )
                    with ex -> printfn "chart %A" ex.Message
                    grid.Visible <- true
                    form.ResumeLayout()
                }
                |> Async.RunSynchronously
            )
        
        disp
        
