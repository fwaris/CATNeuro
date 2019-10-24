namespace CATNeuro
open CATProb
open CAEvolve

module rec HistoryKS =
    let policy =
        [|
            Crossover           , 0.05
            AddNode             , 0.05
            AddConnection       , 0.2
            MutateParm          , 0.2
            ToggleConnection    , 0.5
        |]
        |> createWheel

    let acceptance ca cfg species (st,topG:Individual[]) =
        let hsst = st.HsState
        let hsst' = updateState ca hsst topG
        let st' = {st with HsState=hsst'}
        (st',topG)

    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = 
        let indvs' = indvs |> Array.map (updateIndv cfg speciesType st)
        st,indvs'

    let updateIndv cfg speciesType st (indv:Individual) =
        let hsst = st.HsState
        let rndIndv = hsst.Events.[RNG.Value.Next(hsst.Events.Length)]
        let selIndv = if rndIndv.Fitness.[0] > indv.Fitness.[0] then indv else {rndIndv with Id=indv.Id}
        let influencer = if selIndv = rndIndv then indv else rndIndv
        evolveIndv cfg st speciesType policy (Some influencer) selIndv
        
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

