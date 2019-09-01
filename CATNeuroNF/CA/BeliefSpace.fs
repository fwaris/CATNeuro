namespace CATNeuro
open Ext

module rec BeliefSpace =

    /// CA acceptance function
    let acceptance ca st pop = 
        let indvMap = pop.Individuals |> Array.map (fun ind->ind.Id,ind) |> Map.ofArray

        let pareto = 
            ca.ParetoRank (pop.Individuals |> Array.map (fun ind->ind.Id,ind.Fitness)) 
            |> Array.map (fun id -> indvMap.[id])

        pareto |> Array.take ((float pareto.Length) * ca.Settings.TakeFraction |> int)        
    
    ///CA influence function
    let influence ca st topP pop =
        let st',indvs' = distributeKnowledge ca st pop.Cfg pop.Species pop.Individuals
        let indvKs = indvs' |> Array.groupBy (fun k->k.KS)

        let (st'',indvs'') =
            ((st',[]),indvKs) 
            ||> Array.fold (fun (st,acc) (ks,indvs) ->
                let st',indvs' =
                    match ks with 
                    | Domain        -> domainInfluence      ca st pop.Cfg topP indvs
                    | Situational   -> situationalInfluence ca st pop.Cfg topP indvs
                    | Topgraphical  -> topoInfluence        ca st pop.Cfg topP indvs
                    | Historical    -> historicalInfluence  ca st pop.Cfg topP indvs
                    | Normative     -> normativeInfluence   ca st pop.Cfg topP indvs
                st',indvs'::acc)
            
        let indvs''' = Seq.collect yourself indvs'' |> Seq.sortBy (fun (i:Individual)->i.Id) |> Seq.toArray
        st'',{pop with Individuals=indvs'''}

    let distributeKnowledge  = KDStagHunt.distributeKnowledge

    let domainInfluence         ca st cfg topP indvs = st,indvs
    let situationalInfluence    ca st cfg topP indvs = st,indvs
    let topoInfluence           ca st cfg topP indvs = st,indvs
    let historicalInfluence     ca st cfg topP indvs = st,indvs
    let normativeInfluence      ca st cfg topP indvs = st,indvs

