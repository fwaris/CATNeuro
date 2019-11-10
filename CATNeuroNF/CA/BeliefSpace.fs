namespace CATNeuro
open Ext

module rec BeliefSpace =
    //bindings to implementation
    //let distributeKnowledge     = KDStagHunt.distributeKnowledge
    //let distributeKnowledge     = KDWtdMajority.distributeKnowledge

    let domainInfluence         = DomainKS.influence
    let situationalInfluence    = SituationalKS.influence
    let topoInfluence           = TopographicKS.influence
    let historicalInfluence     = HistoryKS.influence
    let normativeInfluence      = NormativeKS.influence

    let domainAcceptance        = DomainKS.acceptance
    let situationalIAcceptance  = SituationalKS.acceptance
    let topoAcceptance          = TopographicKS.acceptance
    let historicalAcceptance    = HistoryKS.acceptance
    let normativeAcceptance     = NormativeKS.acceptance


    /// CA acceptance function
    let acceptance ca st pop = 
        let indvMap = pop.Individuals |> Array.map (fun ind->ind.Id,ind) |> Map.ofArray

        let pareto = 
            ca.ParetoRank (pop.Individuals |> Array.map (fun ind->ind.Id,ind.Fitness)) 
            |> Array.map (fun id -> indvMap.[id])

        let topG = pareto |> Array.take ((float pareto.Length) * ca.Settings.TakeFraction |> int)        



        //thread state & top indvs through acceptance
        (st,topG)
        |> situationalIAcceptance ca pop.Cfg pop.Species
        |> historicalAcceptance ca pop.Cfg pop.Species
        |> domainAcceptance ca pop.Cfg pop.Species
        |> normativeAcceptance ca pop.Cfg pop.Species
        |> topoAcceptance ca pop.Cfg pop.Species

    ///CA influence function
    let influence ca distributeKnowledge st topP pop =
        let st',indvs' = distributeKnowledge ca pop.Cfg pop.Species st pop.Individuals
        let indvKs = indvs' |> Array.groupBy (fun k->k.KS)
        let speciesType = pop.Species
        let cfg = pop.Cfg

        let (st'',indvs'') =
            ((st',[]),indvKs) 
            ||> Array.fold (fun (st,acc) (ks,indvs) ->
                let st',indvs' =
                    match ks with 
                    | Domain        -> domainInfluence      ca cfg speciesType st topP indvs
                    | Situational   -> situationalInfluence ca cfg speciesType st topP indvs
                    | Topgraphical  -> topoInfluence        ca cfg speciesType st topP indvs
                    | Historical    -> historicalInfluence  ca cfg speciesType st topP indvs
                    | Normative     -> normativeInfluence   ca cfg speciesType st topP indvs
                st',indvs'::acc)
            
        let indvs''' = Seq.collect yourself indvs'' |> Seq.sortBy (fun (i:Individual)->i.Id) |> Seq.toArray
        st'',{pop with Individuals=indvs'''}



