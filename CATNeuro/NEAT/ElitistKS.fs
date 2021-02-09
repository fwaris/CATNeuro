namespace CATNeuro
open Ext
open CATProb
open CAEvolve

module rec ElitistKS = 
    let policy =
        [|
            Crossover           , 0.1       //equal prob for all
            AddNode             , 0.1
            AddConnection       , 0.1
            MutateParm          , 0.1
            ToggleConnection    , 0.1
        |]
        |> createWheel

    let acceptance ca cfg speciesType (st,topG) =
        (st,topG)

    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = 

        let indvs' = 
            indvs
            |> Array.map (fun indv ->
                let elite = topP.[CATProb.RNG.Value.Next(topP.Length)]
                let indv' = evolveIndv cfg st speciesType policy (Some indv) elite
                {indv' with Id=indv.Id}
            )

        st,indvs'

(*
NEAT is elitist so
30% (make this a parameter) of the population is 
replaced with add-node mutations of topG
the rest are add node mutations of existing

*)


