namespace CATNeuro
open Ext
open CATProb
open CAEvolve

module rec DomainKS = 
    let policy =
        [|
            Crossover           , 0.1
            AddNode             , 0.5
            AddConnection       , 0.1
            MutateParm          , 0.5
            ToggleConnection    , 0.1
        |]
        |> createWheel

    let acceptance ca cfg speciesType (st,topG) =
        let st' = {st with DmState = {st.DmState with Gens=st.DmState.Gens+1}} //for future 
        (st',topG)

    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = 
        let takeNum     = (float indvs.Length) * cfg.EliteFraction |> int
        let byFitness   = CAUtils.rankIndvs ca indvs // pareto rank individuals|> Array.sortBy (fun ind -> ind.Fitness.[0])
        let elites      = byFitness |> Array.take takeNum
        let toReplace   = byFitness |> Array.skip takeNum

        let elites' =  elites |> Array.map (fun elite ->
                                    let influencer = topP.[RNG.Value.Next(topP.Length)]
                                    evolveIndv cfg st speciesType policy (Some influencer) elite)

        let reps' = toReplace |> Array.map (fun indv ->
            let topRnd = topP |> Array.item (RNG.Value.Next(topP.Length))
            let indv' = evolveIndv cfg st speciesType policy (Some indv) topRnd
            {indv' with Id=indv.Id}) //preserve id 

        let indvs' = Array.append elites' reps' |> Array.sortBy (fun x->x.Id)
        st,indvs'

(*
domain understands the rules
it could invetigate the elites
and the individual and modify 

Domain adds nodes. Since domain 'understands' the graph structure,
it can add the right kind of nodes.

E.g. for example there should not be two BatchNorm or LayerNorm nodes in
succession. Consider adding BatchNorm between two dense layers
High prob of Norm layer after a dense layer


NEAT is elitist so
30% (make this a parameter) of the population is 
replaced with add-node mutations of topG
the rest are add node mutations of existing

*)

