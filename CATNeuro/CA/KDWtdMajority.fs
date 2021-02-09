namespace CATNeuro
open Ext
open CATProb

///Knowledge distribution via Weighted Majority
module rec KDWtdMajority = 
    let allKSSet = set [Normative; Situational; Domain; Topgraphical; Historical]
    let LOST_KS_WT = 0.20 // percentage of pop that is randomly assigned a KS that was pushed out

    ///entry point for KD
    let distributeKnowledge ca cfg species st pop =
        if st.Gen % 3 = 0 then
            st,wtdMajorityDist ca pop  |> CAUtils.adjustForMaxNodes cfg
        else
            st,pop

    ///distribute knowledge using WTD Majority method
    let wtdMajorityDist ca pop =
        let nrmlzdFit = CAUtils.normalizePopFitness (0.,1.)  pop
        let ksFit = pop |> Array.map(fun indv -> indv.KS, nrmlzdFit.[indv.Id]) |> Array.groupBy fst
        let ksFit = ksFit |> Array.map(fun (ks,kss) -> ks, kss |> Array.sumBy snd)
        let ws,ksWheel = CATProb.createWheelW ksFit                      // weighted spin wheel
        let ksMap = dict ws //ks weight lookup dictionary
        let pop' = pop |> Array.map(wmDist pop ca.Network ksMap ksWheel)
        let missingKS = pop' |> Array.map (fun i -> i.KS) |> set |> Set.difference allKSSet
        for ks in missingKS do
            for _ in 1 .. (float pop'.Length * LOST_KS_WT |> int) do
                let indx = RNG.Value.Next(pop.Length)
                //mutate local copy of pop array
                pop'.[indx] <- {pop.[indx] with KS = ks}
        pop'
       
    ///assign ks to individual
    let wmDist pop network ksMap wheel indv =
        let nbrs = network pop indv.Id
        let directKS = CATProb.spinWheel wheel
        let ksCounts = nbrs |> Array.map (fun i -> i.KS,1) |> Array.append [|directKS,1|]
        let getWeight ks = match ksMap.TryGetValue ks with (true,x) -> x | _ -> 0.
        let wtdKSCnts =    //weighted sum of ks counts for indv + neighbors
            ksCounts 
            |> Array.groupBy fst 
            |> Array.map (fun (ks,xs) -> ks,(xs |> Array.sumBy snd  |> float) * getWeight ks)
        let kd,_ = wtdKSCnts |> Array.maxBy snd
        let possibleConflicts = wtdKSCnts  |> Array.filter (fun (n,c)->n=kd)
        let kd = 
            if possibleConflicts.Length > 1 then
                let (kd,_) = possibleConflicts.[RNG.Value.Next(possibleConflicts.Length)]
                kd
            else
                kd
        {indv with KS = kd}





       
        

