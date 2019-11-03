namespace CATNeuro
open Ext
open CATProb

///Knowledge distribution via Stag Hunt games
module rec KDStagHuntStoch = 

    ///entry point for KD
    let distributeKnowledge ca cfg species st pop =
        let st',pop' =
            if isCompetitiveGen st then
                competitiveDist ca st species pop
            else
                cooperativeDist ca st species pop
        let pop'' = adjustForMaxNodes cfg pop'
        st',pop''

    ///ordering of KS from most exploitative to explorative
    let defaultKSOrder = 
        [|
            Situational  
            Situational   
            Normative
            Historical   
            Topgraphical 
            Domain       
            Domain       
        |]

    ///cache range for scaling
    let ksRange = (0.0, defaultKSOrder.Length |> float)

    ///check if competitive or cooperative phase                      
    let isCompetitiveGen st = st.ShState.GensSinceInit > st.ShState.CoopGens

    ///distribute knowledge for competitive phase
    let competitiveDist ca st species pop = 
        let fitAtInit = st.ShState.FitnessAtInit
        let pop' =
            pop
            |> Array.map (fun indv ->
                if indv.Fitness.[0] < fitAtInit.[indv.Id].[0] then
                    indv //keep KS
                else
                    let frnds = ca.Network pop indv.Id 
                    let all = Array.append frnds [|indv|]
                    let dominantKs,_ = 
                        all 
                        |> Array.groupBy (fun x -> x.KS)
                        |> Array.map(fun (k,xs) -> k,xs|>Array.map(fun y->y.Fitness.[0]) |> Array.sum)
                        |> Array.sortBy snd
                        |> Array.item 0
                    {indv with KS=dominantKs})

        let fitAtInit' = pop' |> Array.map (fun ind->ind.Id,ind.Fitness) |> Map.ofArray

        let shState' = 
            {st.ShState with 
                FitnessAtInit = fitAtInit'
                GensSinceInit = 0
            }

        let st' = {st with ShState = shState'}
        st',pop'

    let fitnessById (i:Individual) = i.Id, i.Fitness
    ///distribute knowledge for cooperative phase
    let cooperativeDist ca st _ (pop:Individual[]) = //assume pop is sorted in order of index
        let pop' =
            pop
            |> Array.map (fun indv ->
                let frnds = ca.Network pop indv.Id 
                let all = Array.append frnds [|indv|]
                let ranked = ca.ParetoRank (all |> Array.map fitnessById)
                let myRank = ranked |> Array.findIndex(fun id->id=indv.Id) |> float
                let pX = 1.0 / (float all.Length)
                let myCumDnsty = myRank * pX
                let scaledRank = scaler ksRange (0.0, 1.0) myCumDnsty |> int //best is lower as its usually loss

                //adjust for low rank as discretization is too coarse
                //let scaledRank' = if scaledRank > 0 then scaledRank else if RNG.Value.NextDouble() < 0.5 then 0 else 1

                let newKS = defaultKSOrder.[scaledRank |> min (defaultKSOrder.Length - 1)]
                {indv with KS=newKS} 
               )
        let st' = {st with ShState = {st.ShState with GensSinceInit=st.ShState.GensSinceInit+1}}
        st',pop'
                    

    /// if an individual has Domain KS but has reached
    /// max nodes then switch KS 
    //(Domain adds new nodes which can't be done for such indviduals)
    let adjustForMaxNodes cfg (pop:Individual[]) =
        pop |> Array.map(fun indv -> 
            if indv.Graph.Nodes.Count >= cfg.MaxNodes && indv.KS = Domain then  
                //can't add any more nodes so chose another KS
                {indv with KS = CAUtils.randKS (Some Domain)}
            else
                indv)



       
        

