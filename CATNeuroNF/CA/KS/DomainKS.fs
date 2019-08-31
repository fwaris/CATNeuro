namespace CATNeuro
open Ext
open Probability

module rec DomainKS = 

    let addNode cfg (indv:Individual) = 
        { indv with
            Graph = indv.Graph |> GraphOps.addNode cfg
        }      

    let influece ca st cfg (topP:Individual[]) (indvs:Individual[]) = 
        let takeNum     = (float indvs.Length) * st.DmState.EliteFrac |> int
        let byFitness   = indvs |> Array.sortBy (fun ind -> ind.Fitness.[0])
        let elites      = byFitness |> Array.take takeNum
        let toReplace   = byFitness |> Array.skip takeNum
        let elites'     = elites |> Array.map (addNode cfg)

        let reps' = toReplace |> Array.map (fun indv ->
            let rnd = topP |> Array.item (RNG.Value.Next(topP.Length))
            let g = GraphOps.addNode cfg rnd.Graph  //add node to one of the top performers and use its graph
            {indv with Graph=g })

        let indvs' = Array.append elites' reps'
        st,indvs'


(*
domain understands the rules
it could invetigate the elites
and the individual and modify 

Domain add nodes
NEAT is elitist so
30% (make this a parameter) of the population is 
replaced with add-node mutations of topG
the rest are add node mutations of existing

*)

