namespace CATNeuro
open Probability
open Ext
open BeliefSpace

type TimeStep= {CA:CA ; Best:NetworkAssembly[]; Count:int; State:Map<SpeciesType,CAState>}

module rec CARunner =
    let isBlueprint = function Blueprint _  -> true | _ -> false 
    let moduleId    = function Module x     -> Some x | _ -> None

    ///set of module species referenced by the graph
    let moduleSpecies (g:Graph) = 
        g.Nodes 
        |> Map.toSeq 
        |> Seq.map (fun (_,n) -> n.Type) 
        |> Seq.choose (function (Cell (ModuleSpecies id)) -> Some id | _ -> None)
        |> set

    ///replace module references with selected module individuals
    let replaceWith (moduleMap:Map<int,Graph>) (blueprint:Graph) =
        let replace = function 
            | Cell (ModuleSpecies i) -> Cell (SubGraph moduleMap.[i]) 
            | t                      -> t 
        let nodes' = blueprint.Nodes |> Map.map (fun _ n -> {n with Type = replace n.Type})
        {blueprint with Nodes=nodes'}        

    //assemble a single blueprint with randomly selected individuls from module species
    let assembleNetwork (spcsMap:Map<int,Population>) (blueprint:Individual) =
    
        //species used by blueprint
        let speciesPops = 
            blueprint.Graph 
            |> moduleSpecies 
            |> Seq.map (fun speciesId ->speciesId, spcsMap.Item speciesId)

        //randomly choose a module from each specicies
        let selIndvs = 
            speciesPops 
            |> Seq.map (fun(id,pop) -> id, pop.Individuals.[RNG.Value.Next(pop.Individuals.Length)]) 
            |> Map.ofSeq

        let parms = match blueprint.IndvType with BlueprintIndv p -> p | _ -> failwithf "not a blueprint indvidual"
        let assembly = blueprint.Graph |> replaceWith (selIndvs |> Map.map (fun _ x->x.Graph))
        let replaceMents = selIndvs|>Map.map(fun sid ind->{SpeciesId=sid; IndvidualId=ind.Id}) |> Map.toSeq |> Seq.map snd |> Seq.toArray
        {BlueprintId = blueprint.Id; Parms=parms; Graph=assembly; ModuleReplacements=replaceMents }

    let separatePop pops = 
        let bprints = pops |> Array.find (fun x -> isBlueprint x.Species)
        let spcsMap = pops |> Array.choose (fun x-> moduleId x.Species |> Option.map (fun i->i,x)) |> Map.ofArray
        bprints,spcsMap

    ///assemble blueprints into networks
    let assembleNetworks (ca:CA) =
        let bprints,spcsMap = separatePop ca.Populations 
        bprints.Individuals |> Array.map (assembleNetwork spcsMap)
    
    ///attribute fitness to blueprint and module individuals after evaluation
    let attributeFitness (ca:CA) (networks:NetworkAssembly[]) (evaluated:((int*float[])[])) =
        let fmap = evaluated |> Map.ofArray
        let bprints,spcsMap = separatePop ca.Populations 

        let avgModFit = //fitness of module individuals as the average fitness of the networks where each was used
            networks 
            |> Array.collect (fun n -> n.ModuleReplacements |> Array.map (fun s -> s, fmap.[n.BlueprintId]))
            |> Array.groupBy fst
            |> Array.map (fun (m,fits) -> m, fits |> Array.map snd |> separate |> Array.map Array.average)
            |> Map.ofArray

        let spcs' =
            spcsMap
            |> Map.toSeq
            |> Seq.map(fun (spid,pop) ->
                {pop with 
                    Individuals = 
                        pop.Individuals 
                        |> Array.map (fun indv -> 
                            {indv with 
                                Fitness = 
                                    match avgModFit |> Map.tryFind {SpeciesId=spid; IndvidualId=indv.Id} with
                                    | Some f -> f
                                    | None   -> indv.Fitness})})
            |> Seq.toArray
            
        let bprints' = {bprints with Individuals = bprints.Individuals |> Array.map (fun indv -> {indv with Fitness =  fmap.[indv.Id]})}
        {ca with Populations=Array.append [|bprints'|] spcs'}


    ///step through CA for each population
    let stepPopulations  (st:Map<SpeciesType,CAState>) (ca:CA) = 
        let (st',pop') =
            ((st,[]),ca.Populations)
            ||> Array.fold (fun (st,acc) pop -> 
                let popSt = st.[pop.Species]
                let popSt',topG = acceptance ca popSt pop
                let popSt'',pop' = influence ca popSt' topG pop
                let st' = st |> Map.add pop.Species popSt''
                st',pop'::acc)
        (st',{ca with Populations=pop' |> List.toArray})
    
     ///single timestep 
    let step (st:TimeStep) =
        async {
            let networks = assembleNetworks st.CA
            let! evaluated = networks |> Array.map (st.CA.Evaluator) |> Async.Parallel 
            let nmap = networks |> Array.map (fun x->x.BlueprintId,x) |> Map.ofArray
            let ca' = attributeFitness st.CA networks evaluated
            let state',ca'' = stepPopulations st.State ca'
            let rankedNetworks = st.CA.ParetoRank evaluated |> Array.map (fun i->nmap.[i])
            return 
                {st with 
                    Best=rankedNetworks |> Array.truncate 5; 
                    CA = ca''
                    Count=st.Count+1
                    State = state'
                }
        }
