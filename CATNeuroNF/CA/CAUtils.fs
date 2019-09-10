namespace CATNeuro
//internal state and ops to run CA 
open Probability
open FSharp.Reflection

type ShState = 
    {
        FitnessAtInit   : Map<int,float[]>
        GensSinceInit   : int
        CoopGens        : int
    }

type HsState = {Events:Individual list; Window:int}

type DmState = {EliteFrac:float; NormNodeProb:float}
type SiState = {Exemplars:Individual[]; SpinWheel:(Graph*float)[]}

type CaseWheel = (UnionCaseInfo*float)[]
type IntWheel = (int*float)[]
type ParmType = PDims | PActivation | PBias | PSpecies | PNorm
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

module CAUtils =

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
        

    let initState() =
        {
            Gen = 0
            ShState = {
                        FitnessAtInit   = Map.empty
                        GensSinceInit   = 0
                        CoopGens        = 5
                      }            
            HsState = {Events=[]; Window=20}
            DmState  = {EliteFrac=0.3; NormNodeProb=0.1}
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

        let nodeToAdd =
            match speciesType, g.Nodes.[conn.From].Type, g.Nodes.[conn.To].Type with
            //module connection
            | Module _ , Cell(Norm _)   , _
            | Module _ , _              , Cell (Norm _)
            | Module _ , Input          , Output _ 
            | Module _ , Input          , Cell (Norm _) 
            | Module _ , Cell (Norm _)  , Output _                       -> GraphOps.genDenseCell cfg
            | Module _ , Input          , Cell (Dense _)
            | Module _ , Cell (Dense _) , Cell (Dense _)
            | Module _ , Cell (Dense _) , Output _                       -> denseOrNorm frac cfg
            //blueprint connection
            | Blueprint, Input                  , Output _                
            | Blueprint, Cell(ModuleSpecies _)  , Output _
            | Blueprint, Input                  , Cell (ModuleSpecies _)
            | Blueprint, Cell (ModuleSpecies _) , Cell (ModuleSpecies _) -> GraphOps.genBlueprintCell cfg
            //any other combination is invalid
            | m        , f                      , t  -> failwithf "invalid connection %A %A %A" m f t

        GraphOps.insertNode cfg g conn nodeToAdd


    let randMutation cfg speciesType st (indv:Individual) =
        let spin = RNG.Value.NextDouble()
        let availMtns = 4.0
        let ths = [| for i in 1.0 .. availMtns - 1.0 -> i*(1.0/availMtns) |]
        let g = indv.Graph
        let g' =
            if spin < ths.[0]   then GraphOps.randMutateParm cfg g
            elif spin < ths.[1] then GraphOps.toggleConnection  cfg g
            elif spin < ths.[2] then GraphOps.addConnection cfg g
            else                     insertNode cfg speciesType st g
        {indv with Graph=g'}
