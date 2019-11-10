namespace CATNeuro
//internal state and ops to run CA 
open CATProb
open FSharp.Reflection
open Ext
open System

type ShState = 
    {
        FitnessAtInit   : Map<int,float[]>
        GensSinceInit   : int
        CoopGens        : int
    }

type HsState = {Events:Individual list; Window:int}

type DmState = {EliteFrac:float; NormNodeProb:float}
type SiState = {Exemplars:Individual[]; SpinWheel:(Graph*float)[]}

type CaseWheel = {Samples:int; CWheel:(UnionCaseInfo*float)[]}
type IntWheel = {Samples:int; IWheel:(int*float)[]}
type ParmType = PDims | PActivation | PBias | PSpecies | PNorm | PLearnRate
type ClassInfo = {TotalClasses:int; Refs:int list}
type DistVal = Case of UnionCaseInfo | Cont of float | Class of ClassInfo
type Dist = Cases of CaseWheel | Density of float[] | Classes of IntWheel

type NmState =
    {
        TopIndv : Individual[]
        MaxIndv : int
        Norms   : Map<int,Map<ParmType,Dist>> //norms organized by innovation#
    }

type Centroid =
    {
        Center  : Individual
        Count   : int
        Best    : Individual
    }

type TpState = 
    {
        Centroids : Centroid list
        CIndvs    : Individual[]
        SpinWheel : (Centroid*float)[]
    }

type CAState = 
    {
        Gen     : int
        ShState : ShState
        HsState : HsState
        DmState : DmState
        NmState : NmState
        TpState : TpState
        SiState : SiState
    }


module MUtils =
    let popId = function Blueprint -> CnMetrics.B | Module i -> CnMetrics.M i

module CAUtils =

    let HIGH_VAL = 1e10 //max value of fitness


    ///select a random Knowledge, excluding 'ex' if given
    let randKS (ex:Knowledge option) : Knowledge = 
        let kss = FSharp.Reflection.FSharpType.GetUnionCases(typeof<Knowledge>)
        let exVal = ex |> Option.map (fun a-> FSharp.Reflection.FSharpValue.GetUnionFields(a,typeof<Knowledge>) |> fst) 
        let kss = exVal |> Option.map (fun x -> kss |> Array.filter (fun y-> x=y |> not)) |> Option.defaultValue kss
        let ks = kss.[RNG.Value.Next(kss.Length)]
        FSharp.Reflection.FSharpValue.MakeUnion(ks,[||]) :?> _

    ///pareto rank the given individuals using the CA pareto-rank function
    let rankIndvs  (ca:CA) (indvs:Individual seq) =
        let map = indvs |> Seq.map (fun i -> i.Id,i) |> Map.ofSeq
        let ids = indvs |> Seq.map (fun i -> i.Id,i.Fitness) |> Seq.toArray
        let rIds = ca.ParetoRank ids
        rIds |> Array.map (fun i -> map.[i])

    ///choose a species at random
    let randSpecies numSpecies = RNG.Value.Next(numSpecies)

    ///sample from kernel density estimate
    //https://stats.stackexchange.com/questions/321542/how-can-i-draw-a-value-randomly-from-a-kernel-density-estimate
    let sampleDensity bandwidth (mass:float[]) =  
        let n = mass.[RNG.Value.Next(mass.Length)]
        let k = GAUSS 0.0 bandwidth
        n + k
        

    let initState (pop:Population) =
        {
            Gen = 0
            ShState = {
                        FitnessAtInit   = pop.Individuals |> Array.map (fun i->i.Id,i.Fitness) |> Map.ofArray
                        GensSinceInit   = 0
                        CoopGens        = 5
                      }            
            HsState = {Events=[]; Window=50}
            DmState  = {EliteFrac=0.5; NormNodeProb=0.1}
            NmState = {
                        TopIndv = Array.empty
                        MaxIndv = 50
                        Norms   = Map.empty
                      }

            TpState = {
                        Centroids = []
                        CIndvs    = [||]
                        SpinWheel = [||]
                      }
            SiState = {Exemplars=[||]; SpinWheel=[||]}
            
        }


    ///generate dense or normalization cell
    ///using configured probability
    let denseOrNorm frac cfg = 
        if RNG.Value.NextDouble() <= frac then 
            GraphOps.genNormCell cfg
        else
            GraphOps.genDenseCell cfg

    ///insert a new node to the graph by splitting a random connection
    ///implments rules for the node type to generate
    ///given graph type and selected connection node types
    let insertNode cfg speciesType frac (g:Graph) =
        let conn = GraphOps.randConn g //randomly selected connection that is to be split

        let isCell = function  Cell (Dense _) -> true | _ -> false
        

        let nodeToAdd =
            match speciesType, g.Nodes.[conn.From].Type, g.Nodes.[conn.To].Type with
            //module connection
            | Module _ , Cell(Norm _)   , _
            | Module _ , _              , Cell (Norm _)
            | Module _ , ModInput       , ModOutput
            | Module _ , ModInput       , Cell (Norm _) 
            | Module _ , Cell (Norm _)  , ModOutput                      -> GraphOps.genDenseCell cfg
            | Module _ , ModInput       , Cell (Dense _)
            | Module _ , Cell (Dense _) , Cell (Dense _)
            | Module _ , Cell (Dense _) , ModOutput                      -> denseOrNorm frac cfg
            //blueprint connection
            | Blueprint, Input _                , Output _                
            | Blueprint, Cell(ModuleSpecies _)  , Output _
            | Blueprint, Input _                , Cell (ModuleSpecies _)
            | Blueprint, Cell (ModuleSpecies _) , Cell (ModuleSpecies _) -> GraphOps.genBlueprintCell cfg
            //any other combination is invalid
            | m        , f                      , t  -> failwithf "invalid connection %A %A %A" m f t
        if speciesType=Blueprint && isCell nodeToAdd.Type then
            failwith "invalid node"
        GraphOps.insertNode cfg g conn nodeToAdd


    let randMutation cfg speciesType st (indv:Individual) =
        let spin = RNG.Value.NextDouble()
        let availMtns = 4.0
        let ths = [| for i in 1.0 .. availMtns - 1.0 -> i*(1.0/availMtns) |]
        let g = indv.Graph
        if spin < ths.[0]   then 
            {indv with Graph = GraphOps.randMutateParm cfg g}
        elif spin < ths.[1] then 
            GraphOps.toggleConnection  cfg g 
            |> Option.map (fun g -> {indv with Graph=g}) 
            |> Option.defaultValue indv
        elif spin < ths.[2] then 
            GraphOps.addConnection cfg g
            |> Option.map (fun g -> {indv with Graph=g}) 
            |> Option.defaultValue indv
        else  
            insertNode cfg speciesType st g  
            |> Option.map (fun g -> {indv with Graph=g}) 
            |> Option.defaultValue indv
       


    let hexagonNetworkViz (pop:Individual[]) id =
        let rowCount = sqrt (float pop.Length)
        let rowLen = int rowCount
        let r = id / rowLen
        let c = id % rowLen
        let evnCol = if c % 2 = 0 then 1 else -1
        let idxs = 
            [|
                r * rowLen + c-1
                r * rowLen + c+1
                (r-1) * rowLen + c
                (r+1) * rowLen + c
                (r+evnCol) * rowLen + (c-1)
                (r+evnCol) * rowLen + (c+1)
            |]
        idxs |> Array.map (fun i-> pop.[if i < 0 then pop.Length+i else i % pop.Length])

    let private binnedEvaled bins (xs:(int*float[])[]) : int[] =
            //make bins using first objective range
            let min1 = xs |> Array.map (fun (_,xs)->xs.[0]) |> Array.min
            let max1 = xs |> Array.map (fun (_,xs)->xs.[0]) |> Array.max
            let bins = makeBins min1 (max1 + 0.0001) (float bins)
    
            //bin by first objective
            let binned = 
                (Map.empty, xs |> Array.mapi (fun i indv ->i,indv)) 
                ||> Array.fold (fun acc (i,((_,xs) as indv)) -> 
                    let d = xs.[0]
                    let b = bins |> List.find (fun (mn,mx) -> mn <= d && d < mx)
                    let x = 
                        match acc |> Map.tryFind b with
                        | Some ls -> indv::ls
                        | None    -> [indv]
                    acc |> Map.add b x)     
    
            let sortedBins = 
                binned 
                |> Map.map (fun k vs -> vs |> List.sortBy (fun (_,xs) -> xs.[1]))    //sort by 2nd objective within each bin
                |> Map.toSeq
                |> Seq.sortBy (fun ((mn,mx),_) -> mx)                               //sort bins in order of first objective
                |> Seq.toList
    
            let ordered = clct [] sortedBins []                                     //collect from bins in a round-robbin fashion
    
            ordered 
            |> List.map (fun (id,_) -> id)
            |> List.toArray


    let normalizePopFitness target (pop:Individual[]) =
        let currentFit = pop |> Array.map (fun p -> 1.0/p.Fitness.[0] ) //convert fitness so that higher is considered better
        let minFit = currentFit |> Array.min
        let maxFit = currentFit |> Array.max
        let scaler = scaler target (minFit,maxFit) 
        currentFit |> Array.map scaler  //scale fitness to target range

    
    /// if an individual has Domain KS but has reached
    /// max nodes then switch KS 
    //(Domain adds new nodes which can't be done for such indviduals)
    let adjustForMaxNodes cfg (pop:Individual[]) =
        pop |> Array.map(fun indv -> 
            if indv.Graph.Nodes.Count >= cfg.MaxNodes && indv.KS = Domain then  
                //can't add any more nodes so chose another KS
                {indv with KS = randKS (Some Domain)}
            else
                indv)
    

    let binnedPareto bins (xs:(int*float[])[]) : int[] =
        let evaled =   xs |> Array.filter (fun (x,f) -> f.[0] < HIGH_VAL)
        if evaled |> Array.isEmpty then //its possible that nothing was evaluated yet for a module species
            xs |> Array.map fst
        else 
            let unevaled = xs |> Array.filter (fun (x,f) ->  f.[0] >= HIGH_VAL )   //unevaluated will be sorted last
            let ordered = binnedEvaled bins evaled
            Array.append ordered (unevaled |> Array.map fst)

    
    let scaledPareto (xs:(int*float[])[]) : int[] =
        let topN = float xs.Length * 0.6 |> int
        let xs' = xs |> Array.sortBy (fun (_,fs) -> fs.[0])
        let best = xs'.[0..topN]
        let rest = xs'.[topN+1..]

        let scaledRest = rest |> Array.map (fun (i,fs)->i,(0.5*log(fs.[0])) + log(fs.[1]))
        let sorted = scaledRest |> Array.sortBy (fun (i,sf) -> sf)
        let rslt = Array.append (best |> Array.map fst) (sorted  |> Array.map fst)
        rslt

    let scaledParetoAdapt topPct (xs:(int*float[])[]) : int[] =
        let topN = float xs.Length * 0.6 |> int
        let xs' = xs |> Array.sortBy (fun (_,fs) -> fs.[0])
        let best = xs'.[0..topN]
        let rest = xs'.[topN+1..]

        let scaledRest = rest |> Array.map (fun (i,fs)->i,(0.5*log(fs.[0])) + log(fs.[1]))
        let sorted = scaledRest |> Array.sortBy (fun (i,sf) -> sf)
        let rslt = Array.append (best |> Array.map fst) (sorted  |> Array.map fst)
        rslt
