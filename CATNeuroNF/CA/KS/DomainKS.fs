﻿namespace CATNeuro
open Ext
open Probability

module rec DomainKS = 
    let acceptance ca cfg species (st,topG) =
        (st,topG)

    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = 
        let takeNum     = (float indvs.Length) * st.DmState.EliteFrac |> int
        let byFitness   = CAUtils.rankIndvs ca indvs // pareto rank individuals|> Array.sortBy (fun ind -> ind.Fitness.[0])
        let elites      = byFitness |> Array.take takeNum
        let toReplace   = byFitness |> Array.skip takeNum
        let elites'     = elites |> Array.map (addNode cfg speciesType st)

        let reps' = toReplace |> Array.map (fun indv ->
            let rnd = topP |> Array.item (RNG.Value.Next(topP.Length))
            let g = GraphOps.addNode cfg rnd.Graph  //add node to one of the top performers and use its graph
            {indv with Graph=g })

        let indvs' = Array.append elites' reps'
        st,indvs'

    ///add new node to the individual
    let addNode cfg speciesType st (indv:Individual) = 
        { indv with
            Graph = CAUtils.insertNode cfg speciesType st.DmState.NormNodeProb indv.Graph
        }      

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

