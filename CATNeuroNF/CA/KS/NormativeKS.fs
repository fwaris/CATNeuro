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
Normative understand ranges and distributions.
Tracks these for best performers
Move paramters of influenced indiviuals towards the ranges of best

Parameter types:
- global parameters e.g. learning rate
- cell type
    | Cell (ModuleSpecies a)            -> pick from distribution
    | Cell (Dense a)                    -> pick from distrbiution of dims, bias, activation
    | Cell BatchNorm | Cell LayerNorm   -> pick from distribution between two types

*)


