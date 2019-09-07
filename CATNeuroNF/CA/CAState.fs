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

type CaseWheel = (UnionCaseInfo*float)[]
type IntWheel = (int*float)[]
type ParmType = PDims | PActivation | PBias | PSpecies | PNorm
type DistVal = Case of UnionCaseInfo | Cont of float | Class of int
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
        spinWheel : (Centroid*float)[]
    }

type CAState = 
    {
        Gen     : int
        ShState : ShState
        HsState : HsState
        DmState : DmState
        NmState : NmState
        TpState : TpState
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
    let rankIndvs  (ca:CA) (indvs:Individual[]) =
        let map = indvs |> Array.map (fun i -> i.Id,i) |> Map.ofArray
        let ids = indvs |> Array.map (fun i -> i.Id,i.Fitness)
        let rIds = ca.ParetoRank ids
        rIds |> Array.map (fun i -> map.[i])


