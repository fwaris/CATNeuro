namespace CATNeuro

open Ext
open CATProb
open Microsoft.FSharp.Reflection
open CnMetrics

module rec CAEvolve =
    let log msg = postAll msg
    let logMutation s = log (ParmMutate (MUtils.popId s))
    let logTggle s = log (CnnTggle (MUtils.popId s))
    let logTggleMiss s = log (CnnTggleMiss (MUtils.popId s))
    let logAddCnn s = log (ConnAdd (MUtils.popId s))
    let logAddCnnMiss s = log (ConnAddMiss (MUtils.popId s))
    let logAddNode s = log (NodeAdd (MUtils.popId s))
    let logAddNodeMiss s = log (NodeAddMiss (MUtils.popId s))
    let logXfer s = log (Xfer (MUtils.popId s))
    let logXferMiss s = log (XferMiss (MUtils.popId s))

    
    module EvolveParm =
        let META_INV = -1  //innovation # used as key in look table

        let reify<'a> v = FSharpValue.MakeUnion(v,[||]) :?> 'a

        let mass        = function Density fs  -> fs  | _ -> failwith "density expected"
        let caseWheel   = function Cases whl   -> whl | _ -> failwith "cases expected"
        let classWheel  = function Classes whl -> whl | _ -> failwith "classes expected"

        let updateMeta cfg nmst indv : Individual =
            match indv.IndvType with
            | BlueprintIndv lr -> let lr' = updateMetaParm cfg nmst lr  
                                  {indv with IndvType=BlueprintIndv lr'}
            | _                -> indv

        let clamp mn mx v = v |> max mn |> min mx

        let clampR range v = clamp range.Lo range.Hi v 

        let bandwidth range = (range.Hi - range.Lo) / range.Divisions

        let sampleCont cfg pm parm range = 
            let bw = bandwidth range
            let clmp = clampR range
            pm 
            |> Map.tryFind parm
            |> Option.map mass  
            |> Option.bind (fun xs->if xs.Length >= cfg.SamplingWarmUp then Some(xs) else None)  //don't use distribution if only 1 point in set
            |> Option.map (CAUtils.sampleDensity bw >> clmp >> int)             //sample from kernel density estimate
            |> Option.defaultValue (range |> GraphOps.randRange)                //sample from uniform, if None

        let sampleActivation cfg pm parm  = 
            pm 
            |> Map.tryFind parm
            |> Option.map caseWheel
            |> Option.bind (fun w -> if w.Samples < cfg.SamplingWarmUp || w.Samples < w.CWheel.Length then None else Some w) //if not enough samples, don't pass dist
            |> Option.map (fun w -> w.CWheel |> spinWheel |> reify)                        //sample from dist
            |> Option.defaultValue (GraphOps.randActivation None)                          //random, if None

        let sampleBias cfg pm parm =
            pm 
            |> Map.tryFind parm 
            |> Option.map caseWheel
            |> Option.bind (fun w -> if w.Samples < cfg.SamplingWarmUp || w.Samples < w.CWheel.Length then None else Some w) //if not enough samples, don't pass dist
            |> Option.map (fun w -> w.CWheel |> spinWheel |> reify)                        //sample from dist
            |> Option.defaultValue (GraphOps.randBias())                                   //random, if None

        let sampleSpecies cfg pm parm =
            pm 
            |> Map.tryFind parm 
            |> Option.map classWheel
            |> Option.bind (fun w -> if w.Samples < cfg.SamplingWarmUp || w.Samples < w.IWheel.Length then None else Some w)  //if not enough samples, don't pass dist
            |> Option.map (fun w -> w.IWheel |> spinWheel |> ModuleSpecies)                 //sample from dist
            |> Option.defaultValue (cfg.NumSpecies |> CAUtils.randSpecies |> ModuleSpecies) //random, if None

        let sampleNorm nt pm parm =
            pm 
            |> Map.tryFind parm
            |> Option.map caseWheel
            |> Option.map (fun w -> w.CWheel |> spinWheel |> reify |> Norm)                  //sample from dist
            |> Option.defaultValue (GraphOps.randNormalization (Some nt) |> Norm)            //random, if None

        let updateMetaParm cfg nmst lr =
            let clampR = clamp cfg.LearnRange.Lo cfg.LearnRange.Hi
            let bw = (cfg.LearnRange.Hi - cfg.LearnRange.Lo) / cfg.LearnRange.Divisions
            let lr' =
                nmst.Norms
                |> Map.tryFind META_INV
                |> Option.bind(fun m -> 
                    m
                    |> Map.tryFind PLearnRate
                    |> Option.map mass  
                    |> Option.bind (fun xs->if Array.length xs >= cfg.SamplingWarmUp then Some(xs) else None) //don't use distribution if only 1 point in set
                    |> Option.map (CAUtils.sampleDensity bw >> clampR))   //sample from kernel density estimate 
                |> Option.defaultValue (CATProb.GAUSS lr.Rate 1.0 |> clampR)     //sample from gaussian
            //printfn "learning rate %f" lr'
            {lr with Rate=lr'}

        ///update parameters of the given node by following the 'norms' 
        ///of the best indviduals in the population
        ///each node type and all of its 'named' parameters are considered here
        let updateNode cfg speciesType n (pm:Map<ParmType,Dist>) : Node =
            logMutation speciesType
            let ty =
                match n.Type with

                | Cell (Dense d) -> 
                    let dims = sampleCont cfg pm PDims cfg.DenseRange
                    let acts = sampleActivation cfg pm PActivation
                    let bias = sampleBias cfg pm PBias
                    let d' = {d with Dims=dims; Activation=acts; Bias=bias}
                    Dense d'

                | Cell (ModuleSpecies s ) -> sampleSpecies cfg pm PSpecies

                | Cell (Norm nt) -> sampleNorm nt pm PNorm

                | Cell (Conv2D cv2d) ->
                    let kernel  = sampleCont cfg pm PKernel cfg.KernelRange 
                    let stride  = sampleCont cfg pm PStride cfg.StrideRange 
                    let filters = sampleCont cfg pm PFilters cfg.FiltersRange 
                    let act     = sampleActivation cfg pm PActivationC2D 
                    let cv2d = {cv2d with Kernel=kernel; Stride=stride; Filters=filters; Activation=act}
                    Conv2D cv2d

                | x -> failwithf "case not handled %A" x
            {n with Type = Cell ty}


        let infIndvParms cfg speciesType st indv =
            let nmst = st.NmState
            let nodes' = 
                indv.Graph.Conns
                |> Seq.map (fun c -> c.Innovation, indv.Graph.Nodes.[c.To])
                |> Seq.choose (fun (innovNum,n) -> nmst.Norms |> Map.tryFind innovNum |> Option.map (fun pm->innovNum,n,pm))
                |> Seq.map (fun (i,n,pm) -> updateNode cfg speciesType n pm)
            let gNodes = (indv.Graph.Nodes,nodes') ||> Seq.fold (fun acc n -> acc |> Map.add n.Id n)
            let g = {indv.Graph with Nodes=gNodes}
            let indv' = updateMeta cfg nmst indv
            {indv' with Graph=g}

    module EvolveGraph = 

        let insertNode cfg speciesType st (indv:Individual) =
            CAUtils.insertNode cfg speciesType indv.Graph
            |> Option.map (fun g -> 
                match GraphOps.tryValidate g with
                | Choice1Of2 _  -> logAddNode speciesType
                                   {indv with Graph=g}
                | Choice2Of2 ex -> logAddNodeMiss speciesType
                                   printfn "insertNode: %s" ex
                                   indv)
            |> Option.defaultWith (fun () ->
                logAddNodeMiss speciesType
                {indv with Restrictions=indv.Restrictions.Add AddNode})


        let toggleConnection cfg speciesType (indv:Individual) =
            GraphOps.toggleConnection cfg indv.Graph
            |> Option.map (fun g ->
                match GraphOps.tryTrimGraph g with
                | Choice1Of2 _ -> logTggle speciesType
                                  {indv with Graph=g}
                | Choice2Of2 e -> logTggleMiss speciesType
                                  printfn "toggle connection: %s" e
                                  indv)
            |> Option.defaultWith (fun () ->
                logTggleMiss speciesType
                indv)

        let addConnection cfg speciesType (indv:Individual) =
            GraphOps.addConnection cfg indv.Graph
            |> Option.map(fun g -> 
                match GraphOps.tryTrimGraph g with
                | Choice1Of2 _ -> logAddCnn speciesType
                                  {indv with Graph=g}
                | Choice2Of2 e -> logAddCnnMiss speciesType
                                  printfn "add connection: empty graph %s" e
                                  indv)
            |> Option.defaultWith (fun () ->
                logAddCnnMiss speciesType 
                if indv.Restrictions.Contains AddNode then
                    {indv with Restrictions = indv.Restrictions.Add AddConnection}  // if cannot add node, then new connections are restricted also
                else
                    indv)
               

        let crossover cfg speciesType (influencer:Individual) (indv:Individual) =
            let g = GraphOps.crossover cfg influencer.Graph indv.Graph
            let g' = 
                match GraphOps.tryTrimGraph g with
                | Choice1Of2 _ -> logXfer speciesType;      g
                | Choice2Of2 ex -> logXferMiss speciesType; printfn "Crossover empty graph %s" ex; indv.Graph
            {indv with Graph=g'}


    let evolveIndv cfg (state:CAState) speciesType policy influencer indv =
        let policy' = policy |> Array.filter (fun (m,w) -> indv.Restrictions.Contains m |> not) |> createWheel
        if Array.isEmpty policy' then 
            EvolveParm.infIndvParms cfg speciesType state indv
        else
            let m = spinWheel policy'
            match m with
            | MutateParm           -> EvolveParm.infIndvParms cfg speciesType state indv
            | ToggleConnection     -> EvolveGraph.toggleConnection cfg speciesType indv
            | AddConnection        -> EvolveGraph.addConnection cfg speciesType indv
            | AddNode              -> EvolveGraph.insertNode cfg speciesType state indv
            | Crossover            -> match influencer with 
                                      | Some inf -> EvolveGraph.crossover cfg speciesType inf indv 
                                      | None -> failwithf "crossover requires influencer individual"
 
                    
