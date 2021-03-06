﻿namespace CATNeuro
open Ext
open CATProb

///Knowledge distribution via Stag Hunt games
module rec _KDStagHunt = 
   
    //probability of choosing KS by individual rank
    //lower rank -> more exploitative; higher rank -> more explorative
    let kdDistByRank =
        [
            0, [|                    
                Situational     , 0.7
                Normative       , 0.2
                Historical      , 0.0
                Topgraphical    , 0.0
                Domain          , 0.3
               |]

            1, [|                    
                Situational     , 0.2
                Normative       , 0.7
                Historical      , 0.0   
                Topgraphical    , 0.0
                Domain          , 0.3
               |]

            2, [|                    
                Situational     , 0.5
                Normative       , 0.5
                Historical      , 0.1   
                Topgraphical    , 0.0
                Domain          , 0.3
               |]

            3, [|                    
                Situational     , 0.1
                Normative       , 0.2
                Historical      , 0.5   
                Topgraphical    , 0.0
                Domain          , 0.2
               |]

            4, [|                    
                Situational     , 0.0
                Normative       , 0.1
                Historical      , 0.1   
                Topgraphical    , 0.7
                Domain          , 0.5
               |]

            5, [|                    
                Situational     , 0.0
                Normative       , 0.1
                Historical      , 0.0   
                Topgraphical    , 0.2
                Domain          , 0.7
               |]
            6, [|                    
                Situational     , 0.0
                Normative       , 0.1
                Historical      , 0.1 //backtrack  
                Topgraphical    , 0.2
                Domain          , 0.7
               |]

        ]
        |> List.map (fun (r,w) -> r, createWheel w)
        |> Map.ofList

    ///entry point for KD
    let distributeKnowledge ca cfg species st pop =
        let st',pop' =
            if isCompetitiveGen st then
                competitiveDist ca st species pop
            else
                cooperativeDist ca st species pop
        let pop'' = adjustForMaxNodes cfg pop'
        st',pop''

    /////cache range for scaling
    //let ksRange = (0.0, defaultKSOrder.Length |> float)

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
    let cooperativeDist ca st speciesType (pop:Individual[]) = //assume pop is sorted in order of index
        let pop' =
            pop
            |> Array.map (fun indv ->
                let frnds = ca.Network pop indv.Id 
                let all = Array.append frnds [|indv|]
                let ranked = ca.ParetoRank (all |> Array.map fitnessById)
                let myRank = ranked |> Array.findIndex(fun id->id=indv.Id)

                let adjWheel = 
                    kdDistByRank.[myRank] 
                    |> Array.filter (fun (k,_) -> 
                        if k = Normative then    //don't exclude normative as its needed for parameter evaluation
                            true 
                        else k <> indv.KS)  

                let newKS = adjWheel |> createWheel |> spinWheel

                //match speciesType with Blueprint -> printfn "%d -> %A" myRank newKS | _ -> ()
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



       
        


