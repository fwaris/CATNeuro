namespace CATNeuro
open Ext
open Probability

module rec DomainKS = 
    let acceptance ca cfg species (st,topG) =
        (st,topG)

    ///add new node to the individual
    let addNode cfg speciesType st (indv:Individual) = 
        { indv with
            Graph = insertNode cfg speciesType st indv.Graph
        }      

    ///insert a new node to the graph by splitting a random connection
    ///implments rules for the node type to generate
    ///given graph type and selected connection node types
    let insertNode cfg speciesType st (g:Graph) =
        let conn = GraphOps.randConn g //randomly selected connection that is to be split

        let nodeToAdd =
            match speciesType, g.Nodes.[conn.From].Type, g.Nodes.[conn.To].Type with
            //module connection
            | Module _ , Cell(Norm _)   , _
            | Module _ , _              , Cell (Norm _)
            | Module _ , Input          , Output _ 
            | Module _ , Input          , Cell (Norm _) 
            | Module _ , Cell (Norm _)  , Output _                      -> GraphOps.genDenseCell cfg
            | Module _ , Cell (Dense _) , Cell (Dense _)                -> denseOrNorm st cfg
            //blueprint connection
            | Blueprint, Cell(ModuleSpecies _)  , Output _
            | Blueprint, Input                  , Cell (ModuleSpecies _)
            | Blueprint, Cell (ModuleSpecies _) , Cell (ModuleSpecies _) -> GraphOps.genBlueprintCell cfg
            //any other combination is invalid
            | m        , f                      , t  -> failwithf "invalid connection %A %A %A" m f t

        GraphOps.insertNode cfg g conn nodeToAdd

     
     ///generate dense or normalization cell
     ///using configured probability
    let denseOrNorm st cfg = 
        if RNG.Value.NextDouble() <= st.DmState.NormNodeProb then 
            GraphOps.genNormCell cfg
        else
            GraphOps.genDenseCell cfg

    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = 
        let takeNum     = (float indvs.Length) * st.DmState.EliteFrac |> int
        let byFitness   = indvs |> Array.sortBy (fun ind -> ind.Fitness.[0])
        let elites      = byFitness |> Array.take takeNum
        let toReplace   = byFitness |> Array.skip takeNum
        let elites'     = elites |> Array.map (addNode cfg speciesType st)

        let reps' = toReplace |> Array.map (fun indv ->
            let rnd = topP |> Array.item (RNG.Value.Next(topP.Length))
            let g = GraphOps.addNode cfg rnd.Graph  //add node to one of the top performers and use its graph
            {indv with Graph=g })

        let indvs' = Array.append elites' reps'
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

