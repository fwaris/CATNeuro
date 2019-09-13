namespace CATNeuro
open CATProb

module rec HistoryKS = 
    let acceptance ca cfg species (st,topG:Individual[]) =
        let hsst = st.HsState
        let hsst' = updateState ca hsst topG
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

    let updateState ca hsst topP =
        let cndateBest = CAUtils.rankIndvs ca topP |> Array.head
        let newBest = 
            match hsst.Events with 
            | []                                               -> [cndateBest] //no history
            | x::_ when cndateBest.Fitness.[0] < x.Fitness.[0] -> [cndateBest] //cndt is better
            | _                                                -> []           //cndt is not better than prev best
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

