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
Normative understand ranges.
Tracks parameter ranges of best performers
Move paramters of indiviuals towards the ranges
Each paramter must have type so similar parameters
can be matched across the various genomes
if no match is found, a random mutation is performed
*)


