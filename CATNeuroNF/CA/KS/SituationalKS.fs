namespace CATNeuro
open Ext

module rec SituationalKS = 

    let addNode cfg (indv:Individual) = 
        { indv with
            Graph = indv.Graph |> GraphOps.addNode cfg
        }


    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = st,indvs
    

(*
Situational tracks examplars 
and performs add connection mutation 
to elite and population
*)

