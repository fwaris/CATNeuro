namespace CATNeuro
open Ext

module rec NormativeKS = 

    let addNode cfg (indv:Individual) = 
        { indv with
            Graph = indv.Graph |> GraphOps.addNode cfg
        }
        

    let influence ca st cfg topP indvs = 
        let indvs' = indvs |> Array.map (addNode cfg)
        st,indvs'


(*
domain understands the rules
it could invetigate the elites
and the individual and modify 
*)


