namespace CATNeuro
open Ext

module rec SitutionalKS = 

    let addNode cfg (indv:Individual) = 
        { indv with
            Graph = indv.Graph |> GraphOps.addNode cfg
        }
        

    let influence ca st cfg topP indvs = 
        let indvs' = indvs |> Array.map (addNode cfg)
        st,indvs'


(*
Situational tracks examplars 
and performs add connection mutation 
to elite and population
*)

