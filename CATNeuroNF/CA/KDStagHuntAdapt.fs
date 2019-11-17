namespace CATNeuro
open Ext
open CATProb

///Knowledge distribution via Stag Hunt games
module rec KDStagHunt = 
   
    //probability of choosing KS by individual rank
    //lower rank -> more exploitative; higher rank -> more explorative
    let kdDistByRank =
        [
            0, [|                    
                Normative       , 0.7
                Historical      , 0.3
                Situational     , 0.1
                Topgraphical    , 0.0
                Domain          , 0.0
               |]

            1, [|                    
                Normative       , 0.3
                Historical      , 0.7
                Situational     , 0.3
                Topgraphical    , 0.1
                Domain          , 0.0
               |]

            2, [|                    
                Normative       , 0.1
                Historical      , 0.3
                Situational     , 0.7
                Topgraphical    , 0.3
                Domain          , 0.1
               |]

            3, [|                    
                Normative       , 0.0
                Historical      , 0.1
                Situational     , 0.3
                Topgraphical    , 0.7
                Domain          , 0.3
               |]

            4, [|                    
                Normative       , 0.0
                Historical      , 0.1
                Situational     , 0.3
                Topgraphical    , 0.7
                Domain          , 0.3
               |]

            5, [|                    
                Normative       , 0.0
                Historical      , 0.0
                Situational     , 0.1
                Topgraphical    , 0.3
                Domain          , 0.7
               |]
            6, [|                    
                Normative       , 0.0
                Historical      , 0.0
                Situational     , 0.1
                Topgraphical    , 0.7
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

    let ksOrder = 
        [|
            Normative       
            Historical      
            Situational     
            Topgraphical    
            Domain          
        |]

    //choose next exploitative down from current
    let slideDown ks = 
        let i =  ksOrder |> Array.findIndex (fun x->x=ks )
        let i = if i > 0 then i - 1 else ksOrder.Length - 1
        ksOrder.[i]

    let slideUp ks = 
        let i =  ksOrder |> Array.findIndex (fun x->x=ks )
        let i = (i + 1) % ksOrder.Length
        ksOrder.[i]

    ///distribute knowledge for competitive phase
    let competitiveDist ca st species pop = 
        let fitAtInit = st.ShState.FitnessAtInit
        let pop' =
            pop
            |> Array.map (fun indv ->
                if indv.Fitness.[0] < fitAtInit.[indv.Id].[0] then
                    {indv with KS=slideDown indv.KS}
                else
                    {indv with KS=slideUp indv.KS}
                    //let frnds = ca.Network pop indv.Id 
                    //let all = Array.append frnds [|indv|]
                    //let dominantKs,_ = 
                    //    all 
                    //    |> Array.groupBy (fun x -> x.KS)
                    //    |> Array.map(fun (k,xs) -> k,xs|>Array.map(fun y->y.Fitness.[0]) |> Array.sum)
                    //    |> Array.sortBy snd
                    //    |> Array.item 0
                    //{indv with KS=dominantKs}
                    )

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
        //let pct =  [0.8; 0.7; 0.6; 0.5].[st.ShState.GensSinceInit |> min 3]   //base line
        let pct =  [0.8; 0.7; 0.6; 0.6].[st.ShState.GensSinceInit |> min 3]   
        //let pct =  [0.7; 0.6; 0.4; 0.2].[st.ShState.GensSinceInit |> min 3]
        printfn "pct %f" pct
        let pop' =
            pop
            |> Array.map (fun indv ->
                let frnds = ca.Network pop indv.Id 
                let all = Array.append frnds [|indv|]
                let ranked = CAUtils.scaledParetoAdapt pct (all |> Array.map fitnessById)
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



       
        


