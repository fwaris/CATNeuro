namespace CATNeuro

open Ext
open CATProb
open Microsoft.FSharp.Reflection

module rec CAEvolve =
    
    module EvolveParm =
        let META_INV = -1

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

        let updateMetaParm cfg nmst lr =
            let lr' =
                nmst.Norms.[META_INV] 
                |> Map.tryFind PLearnRate
                |> Option.map mass  
                |> Option.bind (fun xs->if Array.length xs >= 2 then Some(xs) else None) //don't use distribution if only 1 point in set
                |> Option.map (CAUtils.sampleDensity 3.0)   //sample from kernel density estimate
                |> Option.defaultValue (CATProb.GAUSS lr.Rate 1.0 |> clamp cfg.LearnRange.Lo cfg.LearnRange.Hi)     //sample from gaussian
            {lr with Rate=lr'}

        ///update parameters of the given node by following the 'norms' 
        ///of the best indviduals in the population
        ///each node type and all of its 'named' parameters are considered here
        let updateNode cfg n (pm:Map<ParmType,Dist>) : Node =
            let ty =
                match n.Type with
                | Cell (Dense d) -> 
                    let dims = 
                        pm 
                        |> Map.tryFind PDims 
                        |> Option.map mass  
                        |> Option.bind (fun xs->if xs.Length >= 2 then Some(xs) else None) //don't use distribution if only 1 point in set
                        |> Option.map (CAUtils.sampleDensity 3.0 >> int)   //sample from kernel density estimate
                        |> Option.defaultValue (GraphOps.randDims cfg)     //sample from uniform, if None

                    let acts = 
                        pm 
                        |> Map.tryFind PActivation 
                        |> Option.map caseWheel
                        |> Option.map (spinWheel >> reify)                       //sample from dist
                        |> Option.defaultValue (GraphOps.randActivation None)    //random, if None


                    let bias = 
                        pm 
                        |> Map.tryFind PBias 
                        |> Option.map caseWheel
                        |> Option.map (spinWheel >> reify)                      //sample from dist
                        |> Option.defaultValue (GraphOps.randBias())            //random, if None

                    let d' = {d with Dims=dims; Activation=acts; Bias=bias}
                    Dense d'

                | Cell (ModuleSpecies s ) -> 
                    pm 
                    |> Map.tryFind PSpecies 
                    |> Option.map classWheel
                    |> Option.map (fun w -> spinWheel w |> ModuleSpecies)                            //sample from dist
                    |> Option.defaultValue (cfg.NumSpecies |> CAUtils.randSpecies |> ModuleSpecies)  //random, if None

                | Cell (Norm nt) ->
                    pm 
                    |> Map.tryFind PNorm 
                    |> Option.map caseWheel
                    |> Option.map (fun w -> spinWheel w |> reify |> Norm)                            //sample from dist
                    |> Option.defaultValue (GraphOps.randNormalization (Some nt) |> Norm)            //random, if None

                | x -> failwithf "case not handled %A" x
            {n with Type = Cell ty}


        let infIndvParms cfg st indv =
            let nmst = st.NmState
            let nodes' = 
                indv.Graph.Conns
                |> Seq.map (fun c -> c.Innovation, indv.Graph.Nodes.[c.To])
                |> Seq.choose (fun (innovNum,n) -> nmst.Norms |> Map.tryFind innovNum |> Option.map (fun pm->innovNum,n,pm))
                |> Seq.map (fun (i,n,pm) -> updateNode cfg n pm)
            let gNodes = (indv.Graph.Nodes,nodes') ||> Seq.fold (fun acc n -> acc |> Map.add n.Id n)
            let g = {indv.Graph with Nodes=gNodes}
            let indv' = updateMeta cfg nmst indv
            {indv' with Graph=g}

    module EvolveGraph = 

        let insertNode cfg speciesType st (indv:Individual) =
            let g = CAUtils.insertNode cfg speciesType st.DmState.NormNodeProb indv.Graph
            match GraphOps.tryValidate g with
            | Choice1Of2 _  ->  (MUtils.popId speciesType,1) |> Metrics.NodeAdd |> Metrics.postAll
                                {indv with Graph=g}
            | Choice2Of2 ex -> printfn "domain invalid graph"; indv


        let toggleConnection cfg (indv:Individual) =
           let g' = GraphOps.toggleConnection cfg indv.Graph
           match GraphOps.tryTrimGraph g' with
           | Choice1Of2 _ -> {indv with Graph=g'}
           | Choice2Of2 e -> printfn "toggle connection: %s" e; 
                             indv

        let addConnection cfg (indv:Individual) =
            let g = GraphOps.addConnection cfg indv.Graph
            let g' = 
                match GraphOps.tryTrimGraph g with
                | Choice1Of2 _ -> g
                | Choice2Of2 ex -> printfn "add connection: empty graph %s" ex; indv.Graph
            {indv with Graph=g'}

        let crossover cfg (influencer:Individual) (indv:Individual) =
            let g = GraphOps.crossover cfg influencer.Graph indv.Graph
            let g' = 
                match GraphOps.tryTrimGraph g with
                | Choice1Of2 _ -> g
                | Choice2Of2 ex -> printfn "Crossover empty graph %s" ex; indv.Graph
            {indv with Graph=g'}

    let evolveIndv cfg (state:CAState) speciesType policy influencer indv =
        let m = spinWheel policy
        match m with
        | MutateParm           -> EvolveParm.infIndvParms cfg state indv
        | ToggleConnection     -> EvolveGraph.toggleConnection cfg indv
        | AddConnection        -> EvolveGraph.addConnection cfg indv
        | AddNode              -> EvolveGraph.insertNode cfg speciesType state indv
        | Crossover            -> match influencer with 
                                  | Some inf -> EvolveGraph.crossover cfg inf indv 
                                  | None -> failwithf "crossover requires influencer individual"
 
                    
