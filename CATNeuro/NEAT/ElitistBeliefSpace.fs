namespace CATNeuro
open Ext

module rec ElitistBeliefSpace =

    let elitistInfluence         = ElitistKS.influence

    let elitistAcceptance        = ElitistKS.acceptance


    /// NEAT acceptance function
    let acceptance ca st pop = 
        let indvMap = pop.Individuals |> Array.map (fun ind->ind.Id,ind) |> Map.ofArray

        let pareto = 
            ca.ParetoRank (pop.Individuals |> Array.map (fun ind->ind.Id,ind.Fitness)) 
            |> Array.map (fun id -> indvMap.[id])

        let topG = pareto |> Array.take ((float pareto.Length) * ca.Settings.TakeFraction |> int)        

        //thread state & top indvs through acceptance
        (st,topG)
        |> elitistAcceptance ca pop.Cfg pop.Species

    ///NEAT influence function
    let influence ca distributeKnowledge st topP pop =
        let st',indvs' = distributeKnowledge ca pop.Cfg pop.Species st pop.Individuals
        let indvKs = indvs' |> Array.groupBy (fun k->k.KS)
        let speciesType = pop.Species
        let cfg = pop.Cfg

        let (st'',indvs'') =
            ((st',[]),indvKs) 
            ||> Array.fold (fun (st,acc) (ks,indvs) ->
                let st',indvs' =  elitistInfluence   ca cfg speciesType st topP indvs
                st',indvs'::acc)
            
        let indvs''' = Seq.collect yourself indvs'' |> Seq.sortBy (fun (i:Individual)->i.Id) |> Seq.toArray
        st'',{pop with Individuals=indvs'''}




