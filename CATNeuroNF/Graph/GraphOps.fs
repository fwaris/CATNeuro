namespace CATNeuro
open Probability

module rec GraphOps =
    let isInput (n:Node) = match n.Type with Input  -> true | _ -> false
    let isOutput (n:Node) = match n.Type with Output _ -> true | _ -> false

    let randBias() = if RNG.Value.NextDouble() < 0.5 then Bias.On else Bias.Off

    ///select a random activation excluding 'ex' if provided
    let randActivation (ex:Activation option) : Activation =
        let activations = FSharp.Reflection.FSharpType.GetUnionCases(typeof<Activation>)
        let exVal = ex |> Option.map (fun a-> FSharp.Reflection.FSharpValue.GetUnionFields(a,typeof<Activation>) |> fst) 
        let activations = exVal |> Option.map (fun x -> activations |> Array.filter (fun y-> x=y |> not)) |> Option.defaultValue activations
        let act = activations.[RNG.Value.Next(activations.Length)]
        FSharp.Reflection.FSharpValue.MakeUnion(act,[||]) :?> _

    ///select a random normalization excluding 'ex' if provided
    let randNormalization (ex:NormalizationType option) : NormalizationType =
        let norms = FSharp.Reflection.FSharpType.GetUnionCases(typeof<NormalizationType>)
        let exVal = ex |> Option.map (fun a-> FSharp.Reflection.FSharpValue.GetUnionFields(a,typeof<NormalizationType>) |> fst) 
        let norms = exVal |> Option.map (fun x -> norms |> Array.filter (fun y-> x=y |> not)) |> Option.defaultValue norms
        let sel =
            if norms.Length = 1 then 
                norms.[0]
            else
                norms.[RNG.Value.Next(norms.Length)]
        FSharp.Reflection.FSharpValue.MakeUnion(sel,[||]) :?> _    

    let randDims cfg = RNG.Value.Next(cfg.DenseRange.Lo, cfg.DenseRange.Hi) |> int

    ///generate a new dense cell
    let genDenseCell cfg =
        let dims = RNG.Value.Next(cfg.DenseRange.Lo, cfg.DenseRange.Hi)
        let bias = if RNG.Value.NextDouble() > 0.25 then On else Off
        let activation = randActivation None
        {
            Id = cfg.IdGen.node() |> Id
            Type =  Cell (Dense{Dims = int dims; Bias=bias; Activation=activation})
        }

    ///generate a new normalization cell
    let genNormCell cfg =
        let ntype = Norm (randNormalization None)
        {
            Id = cfg.IdGen.node() |> Id
            Type =  Cell ntype
        }

    ///generate a new blueprint cell
    let genBlueprintCell cfg =
        let ntype = ModuleSpecies (RNG.Value.Next(cfg.NumSpecies)) //pick a module species at random
        let dims = RNG.Value.Next(cfg.DenseRange.Lo, cfg.DenseRange.Hi)
        {
            Id = cfg.IdGen.node() |> Id
            Type =  Cell ntype
        }

    ///validate graph structure
    let private validate (g:Graph) = 
        let duplicateEdges = g.Conns |> List.countBy (fun c->c.From,c.To) |> List.filter (fun (_,c) -> c>1)
        if duplicateEdges.IsEmpty |> not then failwithf "Invalid graph: duplicate edges %A" duplicateEdges

        let adjM = 
            let m = g.Nodes |> Map.map (fun k _ ->[])
            let m2 = g.Conns |> List.map (fun c->c.From,c.To) |> List.groupBy fst |> List.map (fun (k,xs)->k,xs |> List.map snd)
            (m,m2) ||> List.fold (fun m (n,ls) -> m |> Map.add n ls)

        let toSet = adjM |> Map.toList |> List.collect snd |> set
        let fromSet = g.Nodes |> Map.toList |> List.choose (fun (k,v) -> if toSet |> Set.contains k then None else Some k) |> set
        let nonInpNoIncming = fromSet |> Set.filter (fun i -> isInput g.Nodes.[i] |> not)
        if nonInpNoIncming.IsEmpty |> not then failwithf "Invalid graph; these non inputs have zero incoming connections %A" nonInpNoIncming
        let inputWIncming = toSet |> Set.filter (fun i -> isInput g.Nodes.[i])
        if inputWIncming.IsEmpty |> not then failwithf "Invalid graph; inputs have incoming connections %A" inputWIncming
        
        //all internal nodes should be reachable from input nodes
        let inputs = fromSet
        let nonInputs = g.Nodes |> Map.toList |> List.filter (snd>>isInput>>not) |> List.map fst |> set

        let rec traverse visited n =
            let visited = visited |> Set.add n
            let toVisit = adjM.[n] |> List.filter (fun t -> visited.Contains t |> not)
            (visited, toVisit) ||> List.fold traverse

        let visited = (Set.empty,inputs) ||> Set.fold traverse

        let unvisited = Set.difference nonInputs visited

        if not unvisited.IsEmpty then failwithf "Invalid graph: nodes not reachable %A" unvisited

        adjM,inputs

    ///topologically sort grpah
    let tsort (g:Graph) =
        let adjM,inputs = validate g

        let inDegrees = 
            let m = g.Conns |> List.groupBy (fun x->x.To) |> List.map(fun (k,xs) -> k, xs.Length) |> Map.ofList
            (m,inputs) ||> Set.fold (fun acc id -> acc |> Map.add id 0)
    
        //(int*node) set used as priority q (credit: John Harrop)
        let q = inDegrees |> Seq.map (fun kv -> kv.Value,kv.Key) |> set 
   
        let rec loop acc m q =
            if Set.isEmpty q then 
                acc |> List.rev
            else
                let (dgr,id) as minE = q |> Set.minElement 
                if dgr <> 0 then failwithf "Invalid graph: cycle detected involving node %A" id
                let conns = adjM.[id]
                let q = q |> Set.remove minE
                let (m,q) = ((m,q),conns) ||> List.fold (fun (m,q) toId -> 
                    let inDegree = m |> Map.find toId
                    let q = q |> Set.remove (inDegree,toId)
                    let inDegree = inDegree - 1
                    let m = m |> Map.add toId inDegree
                    let q =
                        if inDegree >= 0 then
                            q |> Set.add (inDegree,toId)
                        else
                            q
                    (m,q))
                loop (id::acc) m q
        loop [] inDegrees q

    ///get a randomly selected connection
    let randConnForToggle (g:Graph) = 
        let inputs = g.Conns |> List.filter (fun c->isInput g.Nodes.[c.From])
        let outputs = g.Conns |> List.filter (fun c->isOutput g.Nodes.[c.To])
        //exclude single input or output connections from getting turned off
        let cnn = if inputs.Length=1 && inputs.[0].On then g.Conns |> List.filter (fun c -> not(c=inputs.[0])) else g.Conns
        let cnn = if outputs.Length=1 && outputs.[0].On then cnn |> List.filter (fun c-> not(c=outputs.[0])) else cnn
        cnn.[Probability.RNG.Value.Next(cnn.Length)]

    let randConn (g:Graph) = g.Conns.[Probability.RNG.Value.Next(g.Conns.Length)]

    ///update the connection list by removing one connection and adding a list of new connections
    let updateConns removeConn addList baseList = 
        baseList
        |> List.filter (fun c -> c <> removeConn)
        |> List.append addList
        |> List.sortBy (fun c->c.Innovation)

    let internalNodes (g:Graph) = 
        g.Nodes 
        |> Map.toSeq 
        |> Seq.map snd 
        |> Seq.filter (isInput>>not)
        |> Seq.filter (isOutput>>not)
        |> Seq.toArray      

    ///return a new possible random connection that does not yet exist (or None if not possible)
    let randUnconn (g:Graph) =
        let ns = tsort g |> List.toArray
        let idxNonInput = ns |> Array.findIndex (fun id->isInput g.Nodes.[id] |> not)
        let connected = g.Conns |> List.map(fun c->c.From,c.To) |> set
        let unconnected = 
            seq {for i in 0..ns.Length-1 do
                    for j in idxNonInput..ns.Length-1 do
                    if i < j then
                        let possibleConn = ns.[i],ns.[j]
                        if connected.Contains possibleConn |> not then
                            yield possibleConn
                }
            |> Seq.toArray
        if unconnected.Length = 0 then
            None
        else
            Some (unconnected.[RNG.Value.Next(unconnected.Length)])


    ///match connection genes by innovation number
    ///in case of same innovation #, left connection is taken
    let diffConn (a:Graph) (b:Graph) =
        let acs = a.Conns |> List.sortBy (fun x -> x.Innovation)
        let bcs = b.Conns |> List.sortBy (fun x -> x.Innovation)
    
        let rec loop acc ls rs =
            match ls,rs with
            | [],[]                                              -> List.rev acc
            | [],r::rest                                         -> loop ((ExtraR r)::acc) ls rest
            | l::rest,[]                                         -> loop ((ExtraL l)::acc) rest rs
            | l::restL,r::restR when l = r                       -> loop ((Same l)::acc) restL restR
            | l::restL,r::restR when l.Innovation = r.Innovation -> loop ((Diff(l,r))::acc) restL restR
            | l::restL,r::restR when l.Innovation < r.Innovation -> loop ((FrmL l)::acc) restL rs
            | _,r::restR                                         -> loop ((FrmR r)::acc) ls restR

        loop [] acs bcs


    ///merge genes of two parents
    ///parent a is considered dominant (fitter)
    let crossover cfg (a:Graph) (b:Graph) =
        let ms = diffConn a b
        let nodes,conns = (([],[]),ms) ||> List.fold (fun (ns,cs) mtch -> 
            match mtch with
            | Same c | Diff (c,_) | FrmL c | ExtraL c -> a.Nodes.[c.From]::a.Nodes.[c.To]::ns,c::cs
            | FrmR c | ExtraR c                       -> b.Nodes.[c.From]::b.Nodes.[c.To]::ns,c::cs
            )
        {a with Nodes=nodes |> List.map (fun n->n.Id,n) |> Map.ofList; Conns=conns}

    ///add node to graph by splitting a connection
    //('complexify' in NEAT)
    let addNode cfg (g:Graph) =
        let conn = randConn g
        let newNode = genDenseCell cfg 
        insertNode cfg g conn newNode

    let insertNode cfg (g:Graph) conn (newNode:Node) =
        if g.Nodes.Count >= cfg.MaxNodes then
            printfn "max nodes reached"
            g
        else
            let forwardConn = {On=true; Innovation=cfg.IdGen.conn(); From=newNode.Id; To=conn.To}
            let backConn = {conn with To=newNode.Id; Innovation=cfg.IdGen.conn()}
            let disConn = {conn with On=false}
            let conns = g.Conns |> updateConns conn [backConn;forwardConn;disConn] 
            {g with Nodes=g.Nodes |> Map.add newNode.Id newNode; Conns=conns}

    ///toggle a random connection
    let toggleConnection cfg (g:Graph) =
        let oldConn = randConnForToggle g
        let newConn = {oldConn with On=not oldConn.On}
        {g with Conns = g.Conns |> updateConns oldConn [newConn]}

    ///add a random connection
    let addConnection cfg (g:Graph) =
       match randUnconn g with 
       | Some (f,t) -> {g with Conns = {On=true; Innovation=cfg.IdGen.conn(); From=f; To=t}::g.Conns}
       | None       -> g

    /// modify some parameter of node to be like that of the template node
    let mimicParm cfg (g:Graph) nodeId tmpltNodeId =
        let na = g.Nodes.[nodeId]
        let nb = g.Nodes.[tmpltNodeId]
        let nType =
            match na.Type,nb.Type with
            | Cell (ModuleSpecies a), Cell (ModuleSpecies b) -> Cell (ModuleSpecies b)
            | Cell (Dense a), Cell (Dense b)                 -> Cell (Dense b)
            | Cell (Norm a), Cell (Norm b)                   -> Cell (Norm b)
            | x,_ -> x
        {g with Nodes=g.Nodes |> Map.add nodeId {na with Type=nType}}

    ///mutate some random parameter of the node
    let evolveParm cfg (g:Graph) nodeId  =
        let na = g.Nodes.[nodeId]
        let nType =
            match na.Type with
            | Cell (ModuleSpecies a) -> Cell (ModuleSpecies (RNG.Value.Next(cfg.NumSpecies)))
            | Cell (Dense a)         -> Cell (Dense {a with Dims=RNG.Value.Next(int cfg.DenseRange.Lo, int cfg.DenseRange.Hi)})
            | Cell (Norm a)          -> Some a |> randNormalization |> Norm |> Cell
            | x -> x

        {g with Nodes=g.Nodes |> Map.add nodeId {na with Type=nType}}


    ///remove dead nodes
    ///only keep nodes that are on the path 
    ///from inputs to outputs
    let trimGraph (g:Graph) =
        let revIds = tsort g |> List.rev |> List.toArray
        let i = revIds |> Array.findIndex (fun id -> g.Nodes.[id] |> isOutput |> not)
        let out,rest = set revIds.[0..i-1], Array.toList revIds.[i..]
    
        let revAdjM = 
            let m = g.Nodes |> Map.map (fun k _ ->[])
            let m2 = g.Conns |> List.filter (fun c->c.On) |> List.map (fun c->c.To,c.From) |> List.groupBy fst |> List.map (fun (k,xs)->k,xs |> List.map snd)
            (m,m2) ||> List.fold (fun m (n,ls) -> m |> Map.add n ls)                    
    
        let rec traverse reachable n =
            let reachable = reachable |> Set.add n
            let incoming = revAdjM.[n]
            (reachable,incoming) ||> List.fold traverse

        let reachable = (Set.empty,out) ||> Set.fold traverse

        let trimConns = g.Conns |> List.filter (fun c -> reachable.Contains c.From && reachable.Contains c.To)
        let trimNodes = g.Nodes |> Map.filter (fun k _ -> reachable.Contains k)
        {g with Nodes=trimNodes; Conns=trimConns}

    ///a measure of distance between two dense cell types
    let distDense (d1:Dense) (d2:Dense) =
        let dist = 
            [
                (if (d1.Activation = d2.Activation) then 0.0 else 1.0)
                (if (d1.Bias = d2.Bias) then 0.0 else 1.0)
                abs ((float d1.Dims) - (float d2.Dims)) / (float (d1.Dims + d2.Dims))
            ]
            |> List.sum
        dist / 3.0

    ///a measure of distance between two cells
    let distCell (c1:CellType) (c2:CellType) =
        let W = 1.0
        match c1, c2 with
        | ModuleSpecies a, ModuleSpecies b when a = b -> 0.0
        | Norm BatchNorm, Norm BatchNorm              -> 0.0
        | Norm LayerNorm, Norm LayerNorm              -> 0.0
        | Dense d1, Dense d2                          -> distDense d1 d2
        | SubGraph s1, SubGraph s2                    -> distGraph s1 s2
        | _, _                                        -> W
       
    ///a measure of distance between two nodes
    let distConn (n1:Node) (n2:Node) =
        let W=2.0
        match n1.Type, n2.Type with
        | Input, Input          -> 0.0
        | Output _, Output _    -> 0.0
        | Cell c1, Cell c2      -> distCell c1 c2
        | _, _                  -> W

    ///a measure of distance between two 
    ///graphs
    let distGraph (g1:Graph) (g2:Graph) = 
        let W = 5.0
        diffConn g1 g2
        |> List.map (
            function 
            | Same c     -> 0.0
            | Diff (l,r) -> distConn g1.Nodes.[l.To] g2.Nodes.[r.To]
            | FrmL c     -> W
            | FrmR c     -> W
            | ExtraR c   -> W
            | ExtraL c   -> W
        )
        |> List.sum



