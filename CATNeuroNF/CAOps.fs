namespace CATNeuro
open Ext
type FitHist = {Id:int; Fitness:float[]}
type ShState = 
    {
        FitnessAtInit   : Map<SpeciesType,FitHist[]>
        GensSinceInit   : int
        CoopGens        : int
        KSOrder         : Knowledge[]
        KSRange         : float*float
    }
 
type CAState = {ShState:ShState}

module rec CAOps =

    let evolve (st:CAState) (ca:CA) = 
        let (st',pop') =
            ((st,[]),ca.Populations)
            ||> Array.fold (fun (st,acc) pop -> 
                let topP = acceptance st ca pop
                let st',pop' = influence st topP pop
                st',pop'::acc)
        (st',{ca with Populations=pop' |> List.toArray})

    let updateState ca st  = st

    let acceptance st ca pop = 
        let indvMap = pop.Individuals |> Array.map (fun ind->ind.Id,ind) |> Map.ofArray

        let pareto = 
            ca.ParetoRank (pop.Individuals |> Array.map (fun ind->ind.Id,ind.Fitness)) 
            |> Array.map (fun id -> indvMap.[id])

        pareto |> Array.take ((float pareto.Length) * ca.Settings.TakeFraction |> int)        
        
    let influence st topP pop =
        let st',indvs' = distributeKnowledge st pop.Individuals
        let indvKs = indvs' |> Array.groupBy (fun k->k.KS)
        let (st'',indvs'') =
            ((st',[]),indvKs) 
            ||> Array.fold (fun (st,acc) (ks,indvs) ->
                let st',indvs' =
                    match ks with 
                    | Domain        -> domainInfluence      pop.Cfg topP st indvs
                    | Situational   -> situationalInfluence pop.Cfg topP st indvs
                    | Topgraphical  -> topoInfluence        pop.Cfg topP st indvs
                    | Historical    -> historicalInfluence  pop.Cfg topP st indvs
                    | Normative     -> normativeInfluence   pop.Cfg topP st indvs
                st',indvs'::acc)
                
         
        let indvs''' = Seq.collect yourself indvs'' |> Seq.sortBy (fun (i:Individual)->i.Id) |> Seq.toArray
        st'',{pop with Individuals=indvs'''}

    let distributeKnowledge st indvs = st,indvs

    let domainInfluence cfg topP st indvs = st,indvs
    let situationalInfluence cfg topP st indvs = st,indvs
    let topoInfluence cfg topP st indvs = st,indvs
    let historicalInfluence cfg topP st indvs = st,indvs
    let normativeInfluence cfg topP st indvs = st,indvs






        