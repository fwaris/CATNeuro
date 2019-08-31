namespace CATNeuro
open Ext

module rec TopographicKS = 

    let addNode cfg (indv:Individual) = 
        { indv with
            Graph = indv.Graph |> GraphOps.addNode cfg
        }
        

    let influence ca st cfg topP indvs = 
        let indvs' = indvs |> Array.map (addNode cfg)
        st,indvs'


(*
Topopgraphic keeps clusters of best performers
and does crossover between clusters
*)


