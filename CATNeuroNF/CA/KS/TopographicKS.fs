namespace CATNeuro
open Ext

module rec TopographicKS = 

    let addNode cfg (indv:Individual) = 
        { indv with
            Graph = indv.Graph |> GraphOps.addNode cfg
        }
        
    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = st,indvs

(*
Topopgraphic keeps clusters of best performers
and does crossover between clusters
*)


