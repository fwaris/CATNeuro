namespace CATNeuro
open Probability

module rec HistoryKS = 
    let acceptance ca cfg species (st,topG) =
        let hsst = st.HsState
        let hsst' = updateState hsst topG
        let st' = {st with HsState=hsst'}
        (st',topG)

    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = 
        let indvs' = indvs |> Array.map (updateIndv cfg st.HsState)
        st,indvs'

    let updateIndv cfg hsst (indv:Individual) =
        let rndIndv = hsst.Events.[RNG.Value.Next(hsst.Events.Length)]
        if rndIndv.Fitness.[0] > indv.Fitness.[0] then
            {indv with Graph=GraphOps.toggleConnection cfg indv.Graph}
        else
            {indv with Graph=GraphOps.toggleConnection cfg rndIndv.Graph}

    let updateState hsst topP =
        let cndateBest = topP |> Array.sortBy (fun indv->indv.Fitness.[0]) |> Array.head
        let newBest = 
            match hsst.Events with 
            | []                                               -> [cndateBest] //no history
            | x::_ when cndateBest.Fitness.[0] < x.Fitness.[0] -> [cndateBest] //cndt is better
            | _                                                -> []
        let events = List.append newBest hsst.Events |> List.truncate hsst.Window
        {hsst with Events=events}

(*
History tracks best across generations.
It's way of back tracking to find promising
branches

History toggles connection 
It add a toggle connection mutation to a randomly
chosen from the best collection
*)

