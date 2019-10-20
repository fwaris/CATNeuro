﻿namespace CATNeuro
open Ext
open CATProb

module rec DomainKS = 
    let acceptance ca cfg speciesType (st,topG) =
        (st,topG)

    let insertNode cfg speciesType st (indv:Individual) =
        let g = CAUtils.insertNode cfg speciesType st.DmState.NormNodeProb indv.Graph
        match GraphOps.tryValidate g with
        | Choice1Of2 _  ->  (MUtils.popId speciesType,1) |> Metrics.NodeAdd |> Metrics.postAll
                            {indv with Graph=g}
        | Choice2Of2 ex -> printfn "domain invalid graph"; indv

    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = 
        let takeNum     = (float indvs.Length) * st.DmState.EliteFrac |> int
        let byFitness   = CAUtils.rankIndvs ca indvs // pareto rank individuals|> Array.sortBy (fun ind -> ind.Fitness.[0])
        let elites      = byFitness |> Array.take takeNum
        let toReplace   = byFitness |> Array.skip takeNum
        let elites'     = elites |> Array.map (insertNode cfg speciesType st)

        let reps' = toReplace |> Array.map (fun indv ->
            let topRnd = topP |> Array.item (RNG.Value.Next(topP.Length))
            insertNode cfg speciesType st topRnd)


        let indvs' = Array.append elites' reps' |> Array.sortBy (fun x->x.Id)
        st,indvs'


(*
domain understands the rules
it could invetigate the elites
and the individual and modify 

Domain adds nodes. Since domain 'understands' the graph structure,
it can add the right kind of nodes.

E.g. for example there should not be two BatchNorm or LayerNorm nodes in
succession. Consider adding BatchNorm between two dense layers
High prob of Norm layer after a dense layer


NEAT is elitist so
30% (make this a parameter) of the population is 
replaced with add-node mutations of topG
the rest are add node mutations of existing

*)

