namespace CATNeuro
open Ext

module rec KDStagHunt = 

    let defaultKSOrder = 
            [|
                Situational  //toggle connection   ///  exploitative to explorative order
                Normative    //evolve parms
                Historical   //add connection
                Domain       //add new node
                Topgraphical //graph union
            |]

    let ksRange = (0.0, defaultKSOrder.Length - 1 |> float)

    let fitnessById (i:Individual) = i.Id, i.Fitness
                           
    let isCompetitiveGen st = st.ShState.GensSinceInit > st.ShState.CoopGens

    let distributeKnowledge ca st species pop =
        if isCompetitiveGen st then
            competitiveDist ca st species pop
        else
            cooperativeDist ca st species pop

    let competitiveDist ca st species pop = 
        let fitAtInit = st.ShState.FitnessAtInit.[species]
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
                FitnessAtInit = st.ShState.FitnessAtInit |> Map.add species fitAtInit'
                GensSinceInit = 0
            }


        let st' = {st with ShState = shState'}
        st',pop'
                    
    let cooperativeDist ca st _ (pop:Individual[]) = //assume pop is sorted in order of index
        let pop' =
            pop
            |> Array.map (fun indv ->
                let frnds = ca.Network pop indv.Id 
                let all = Array.append frnds [|indv|]
                let ranked = ca.ParetoRank (all |> Array.map fitnessById)
                let bestFrnd = all |> Array.find (fun x->x.Id=ranked.[0])
                let wrstFrnd = all |> Array.find (fun x->x.Id=Array.last ranked)

                let bestFit = bestFrnd.Fitness.[0] //used primary objective for placement
                let wrstFit = wrstFrnd.Fitness.[0]
                let myFit = indv.Fitness.[0]

                if isValidNum bestFit && isValidNum wrstFit then
                    let scaledRank = scaler ksRange (bestFit,wrstFit) myFit //best is lower as its usually loss
                    let newKS = defaultKSOrder.[int scaledRank |> min (defaultKSOrder.Length - 1)]
                    {indv with KS=newKS}
                else
                    indv)
        let st' = {st with ShState = {st.ShState with GensSinceInit=st.ShState.GensSinceInit+1}}
        st',pop'


       
        

