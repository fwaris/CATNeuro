namespace CATNeuro
open Ext

module rec HistoryKS = 

    let addNode cfg (indv:Individual) = 
        { indv with
            Graph = indv.Graph |> GraphOps.addNode cfg
        }
        

    let influence ca st cfg topP indvs = 
        let indvs' = indvs |> Array.map (addNode cfg)
        st,indvs'


(*
History tracks best across generations.
It add a toggle connection mutation to a randomly
chosen from the best collection
*)

