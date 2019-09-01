namespace CATNeuro
open Probability
//internal state to run CA 
type ShState = 
    {
        FitnessAtInit   : Map<SpeciesType,Map<int,float[]>>
        GensSinceInit   : int
        CoopGens        : int
    }

type HsState = {Events:Individual list; Window:int}

type DmState = {EliteFrac:float; NormNodeProb:float}
 
type CAState = {ShState:ShState; Gen:int; HsState:HsState; DmState:DmState}

module CAUtils =

    let randKS (ex:Knowledge option) : Knowledge = 
        let kss = FSharp.Reflection.FSharpType.GetUnionCases(typeof<Knowledge>)
        let exVal = ex |> Option.map (fun a-> FSharp.Reflection.FSharpValue.GetUnionFields(a,typeof<Knowledge>) |> fst) 
        let kss = exVal |> Option.map (fun x -> kss |> Array.filter (fun y-> x=y |> not)) |> Option.defaultValue kss
        let ks = kss.[RNG.Value.Next(kss.Length)]
        FSharp.Reflection.FSharpValue.MakeUnion(ks,[||]) :?> _
        


