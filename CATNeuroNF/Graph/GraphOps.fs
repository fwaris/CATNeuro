namespace CATNeuro
open CATProb

module rec GraphOps =
    open System

    let isInput (n:Node) = match n.Type with Input _ | ModInput -> true | _ -> false
    let isOutput (n:Node) = match n.Type with Output _ | ModOutput -> true | _ -> false
    let isNorm (n:Node) = match n.Type with Cell (Norm _) -> true | _ -> false
    let inputName (n:Node) = match n.Type with Input n -> n | _ -> failwith "not named input node"

    let flipCoin() = RNG.Value.NextDouble() < 0.5
    let randBias() = if flipCoin() then Bias.On else Bias.Off
    let randRange r = RNG.Value.Next(int r.Lo, int r.Hi) |> int
    let rollWheel wts = wts |> Array.mapi (fun i x -> i,x) |> createWheel |> spinWheel

    ///select a random activation excluding 'ex' if provided
    let randActivation (ex:Activation option) : Activation =
        let activations = FSharp.Reflection.FSharpType.GetUnionCases(typeof<Activation>)
        let exVal = ex |> Option.map (fun a-> FSharp.Reflection.FSharpValue.GetUnionFields(a,typeof<Activation>) |> fst) 
        let activations = exVal |> Option.map (fun x -> activations |> Array.filter (fun y-> x=y |> not)) |> Option.defaultValue activations
        let act = activations.[RNG.Value.Next(activations.Length)]
        FSharp.Reflection.FSharpValue.MakeUnion(act,[||]) :?> _

    let incomingConnections (g:Graph) nid = g.Conns |> List.filter(fun c->c.To=nid)

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

    ///generate a new dense cell
    let genDenseCell cfg =
        let dims = RNG.Value.Next(int cfg.DenseRange.Lo, int cfg.DenseRange.Hi)
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

    ///generate a new dense cell
    let genConv2DCell cfg =
        let kernel     = cfg.KernelRange  |> randRange 
        let filters    = cfg.FiltersRange |> randRange
        let stride     = cfg.StrideRange  |> randRange
        let activation = randActivation None
        let cell = {Kernel=kernel; Filters=filters; Stride=stride; Activation=activation}
        {
            Id = cfg.IdGen.node() |> Id
            Type =  Cell (Conv2D cell)
        }
        
    ///generate a new blueprint cell
    let genBlueprintCell cfg =
        let ntype = ModuleSpecies (RNG.Value.Next(cfg.NumSpecies)) //pick a module species at random
        let dims = RNG.Value.Next(int cfg.DenseRange.Lo, int cfg.DenseRange.Hi)
        {
            Id = cfg.IdGen.node() |> Id
            Type =  Cell ntype
        }

    let genDenseOrNorm cfg =
        match rollWheel [|cfg.WtSlctn_DenseNode; cfg.WtSlctn_NormNode|] with
        | 0 -> genDenseCell cfg
        | 1 -> genNormCell cfg
        | _ -> failwith "out of range flip value"
        
    let genDenseConvOrNorm cfg =
        match rollWheel [|cfg.WtSlctn_DenseNode; cfg.WtSlctn_CovnNode; cfg.WtSlctn_NormNode|] with
        | 0 -> genDenseCell cfg
        | 1 -> genConv2DCell cfg
        | 2 -> genNormCell cfg
        | _ -> failwith "out of range flip value"

    let genDenseOrConv cfg = 
        match rollWheel [|cfg.WtSlctn_DenseNode; cfg.WtSlctn_CovnNode|] with
        | 0 -> genDenseCell cfg
        | 1 -> genConv2DCell cfg
        | _ -> failwith "out of range flip value"


    ///validate graph structure
    let tryValidate (g:Graph) = 
        if g.Nodes.Count=0 || g.Conns |> List.filter (fun x->x.On) |> List.length = 0 then Choice2Of2 "empty graph"
        else 
            let duplicateEdges = g.Conns |> List.countBy (fun c->c.From,c.To) |> List.filter (fun (_,c) -> c>1)
            if duplicateEdges.IsEmpty |> not then (sprintf "Invalid graph: duplicate edges %A" duplicateEdges) |> Choice2Of2
            else

                let adjM = 
                    let m = g.Nodes |> Map.map (fun k _ ->[])
                    let m2 = g.Conns |> List.map (fun c->c.From,c.To) |> List.groupBy fst |> List.map (fun (k,xs)->k,xs |> List.map snd)
                    (m,m2) ||> List.fold (fun m (n,ls) -> m |> Map.add n ls)

                let toSet = adjM |> Map.toList |> List.collect snd |> set
                let fromSet = g.Nodes |> Map.toList |> List.choose (fun (k,v) -> if toSet |> Set.contains k then None else Some k) |> set
                let nonInpNoIncming = fromSet |> Set.filter (fun i -> isInput g.Nodes.[i] |> not)
                if nonInpNoIncming.IsEmpty |> not then sprintf "Invalid graph; these non inputs have zero incoming connections %A" nonInpNoIncming |> Choice2Of2
                else
                    let inputWIncming = toSet |> Set.filter (fun i -> isInput g.Nodes.[i])
                    if inputWIncming.IsEmpty |> not then sprintf "Invalid graph; inputs have incoming connections %A" inputWIncming |> Choice2Of2
                    else
        
                        //all internal nodes should be reachable from input nodes
                        let inputs = fromSet
                        let nonInputs = g.Nodes |> Map.toList |> List.filter (snd>>isInput>>not) |> List.map fst |> set

                        let rec traverse visited n =
                            let visited = visited |> Set.add n
                            let toVisit = adjM.[n] |> List.filter (fun t -> visited.Contains t |> not)
                            (visited, toVisit) ||> List.fold traverse

                        let visited = (Set.empty,inputs) ||> Set.fold traverse

                        let unvisited = Set.difference nonInputs visited

                        if not unvisited.IsEmpty then sprintf "Invalid graph: nodes not reachable %A" unvisited |> Choice2Of2
                        else

                            Choice1Of2 (adjM,inputs)
    
    exception PossibleCycle of Id

    ///topologically sort graph
    let tsort (g:Graph) =
        let adjM,inputs = match tryValidate g with Choice1Of2 x -> x | Choice2Of2 ex -> failwith ex

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
                if dgr <> 0 then raise (PossibleCycle id)//failwithf "Invalid graph: cycle detected involving node %A" id
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

    let removeLinkTo (g:Graph) id =
        let incomings = g.Conns |> List.filter (fun c->c.To = id)
        if incomings.Length <=1 then 
            printfn "no cycle to eliminate for id %A" id
            None
        else
            let toRmove = incomings |> List.maxBy (fun c->c.Innovation)    //remove last innovation
            Some {g with Conns = g.Conns |> List.filter (fun c-> c <> toRmove)}        

    let eliminateCycle g =
        let rec loop g =
            try
                let _ = tsort g
                g
            with (PossibleCycle id) -> 
                let g' = removeLinkTo g id
                match g' with
                | None -> g
                | Some g -> loop g
        loop g

    let inputNodes (g:Graph) = g.Nodes |> Map.toList |> List.map snd |> List.filter isInput

    let checkDropConn cfg (g:Graph) conn =
        let g' = {g with Conns=updateConns conn [] g.Conns}
        match tryTrimGraph g' with
        | Choice1Of2 g'' -> if cfg.AllowDropInputs then 
                                Some conn 
                            else 
                                if List.length(inputNodes g'') = List.length(inputNodes g) then 
                                    Some conn 
                                else 
                                    None
        | Choice2Of2 _  -> None

    ///get a randomly selected connection
    let randConnForToggle cfg (g:Graph) = 
        let inputs = g.Conns |> List.filter (fun c->isInput g.Nodes.[c.From])
        let outputs = g.Conns |> List.filter (fun c->isOutput g.Nodes.[c.To])

        //safeguard input connections from being turned off, depending on configuration settings
        let cnn = 
            if cfg.AllowDropInputs then
                if inputs.Length=1 && inputs.[0].On then g.Conns |> List.filter (fun c -> not(c=inputs.[0])) else g.Conns
            else
                g.Conns |> List.filter (fun c -> not (isInput g.Nodes.[c.From]))

        let pssblCnns = if outputs.Length=1 && outputs.[0].On then cnn |> List.filter (fun c-> not(c=outputs.[0])) else cnn

        let rec loop cnns =
            if List.length cnns = 0 then
                None
            else
                let cnn = cnn.[CATProb.RNG.Value.Next(cnn.Length)]
                match checkDropConn cfg g cnn with
                | Some cnn -> Some cnn
                | None     ->
                    let cnns' = cnns |> List.filter (fun c -> c <> cnn)
                    loop cnns'

        loop pssblCnns
                

    let randConn (g:Graph) = g.Conns.[CATProb.RNG.Value.Next(g.Conns.Length)]

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

    ///return a random 'possible connection' (i.e. that does not yet exist) or None if not possible
    let randUnconn (g:Graph) =
        let ns = tsort g |> List.toArray     //topological sort to avoid cyclic references
        let connected = g.Conns |> List.map(fun c->c.From,c.To) |> set
        let unconnected = 
            seq {for i in 0..ns.Length-1 do
                    for j in 1..ns.Length-1 do
                    if i < j then
                        match ns.[i],ns.[j] with
                        | f,t when connected.Contains(f,t)                  -> () // already connected
                        | f,t when g.Nodes.[t] |> isInput                   -> () // input
                        | f,t when isNorm g.Nodes.[f] && isNorm g.Nodes.[t] -> () // do not connect two normalization layers
                        | f,t                                               -> yield (f,t)
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

    ///check if 2nd id was created after the 1st id
    let is2ndLater (Id a) (Id b) = 
        match Int32.TryParse a, Int32.TryParse b with 
        | (true,a),(true,b) -> a < b
        | _                 -> false

    ///merge genes of two parents
    ///parent a is considered dominant (fitter)
    let crossover cfg (a:Graph) (b:Graph) =
        let ms = diffConn a b

        let nodes,conns = (([],[]),ms) ||> List.fold (fun (ns,cs) mtch -> 
            match mtch with
            | Same c | Diff (c,_) | FrmL c | ExtraL c -> a.Nodes.[c.From]::a.Nodes.[c.To]::ns,c::cs
            | FrmR c | ExtraR c                       -> b.Nodes.[c.From]::b.Nodes.[c.To]::ns,c::cs
            )

        let inputs = nodes |> List.filter isInput |> List.map(fun i->i.Id) |> set
        let conns' = conns |> List.filter (fun c->inputs.Contains c.To |> not)   //ensure connections point to input

        let connUnique = 
            (Map.empty,conns') 
            ||> List.fold (fun acc c ->
                let c' = 
                    match acc |> Map.tryFind (c.From,c.To) with
                    | Some c2 -> if c2.Innovation < c.Innovation then c2 else c
                    | None    -> c
                acc |> Map.add (c'.From,c'.To) c)
        let conns'' = connUnique |> Map.toSeq |> Seq.map snd |> Seq.toList

        let nodes' = nodes |> List.map (fun n->n.Id,n) |> Map.ofList

        let g = {a with Nodes=nodes' ; Conns=conns''}
        eliminateCycle g

    ///add node to graph by splitting a connection
    //('complexify' in NEAT)
    let addNode cfg (g:Graph) =
        let conn = randConn g
        let newNode = genDenseCell cfg 
        insertNode cfg g conn newNode

    ///toggle a random connection
    let toggleConnection cfg (g:Graph) =
         randConnForToggle cfg g
        |> Option.map(fun oldConn ->
            let newConn = {oldConn with On=not oldConn.On}
            {g with Conns = g.Conns |> updateConns oldConn [newConn]})

    ///add a random connection
    let addConnection cfg (g:Graph) =
        randUnconn g 
        |> Option.map (fun (f,t) -> 
            if isInput g.Nodes.[t] then failwithf "target node is input"
            {g with Conns = {On=true; Innovation=cfg.IdGen.conn(); From=f; To=t}::g.Conns}
        )

    let insertNode cfg (g:Graph) conn (newNode:Node) =
        if g.Nodes.Count >= cfg.MaxNodes then
            printfn "max nodes reached"
            None
        else
            let backConn = {conn with To=newNode.Id; Innovation=cfg.IdGen.conn()}
            let forwardConn = {On=true; Innovation=cfg.IdGen.conn(); From=newNode.Id; To=conn.To}
            let disConn = {conn with On=false}
            let conns = g.Conns |> updateConns conn [backConn;forwardConn;disConn] 
            Some {g with Nodes=g.Nodes |> Map.add newNode.Id newNode; Conns=conns}


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
            | Cell (Dense a)         -> Cell (Dense {a with Dims=randRange cfg.DenseRange})
            | Cell (Norm a)          -> Some a |> randNormalization |> Norm |> Cell
            | Cell (Conv2D cv2d)     -> Cell (Conv2D {cv2d with Filters=randRange cfg.FiltersRange})
            | x                      -> printfn "not mutating node of type %A" x; x

        {g with Nodes=g.Nodes |> Map.add nodeId {na with Type=nType}}

    ///mutate paramenter of a random node
    let randMutateParm cfg (g:Graph) =       
        let ins = internalNodes g 
        if ins.Length > 0 then
            let n = ins.[RNG.Value.Next(ins.Length)]
            evolveParm cfg g n.Id
        else
            printfn "no internal nodes in graph"
            g

    let flip f a b = f b a
    let pop st = match st with [] -> [] | _::tail -> tail

    let incoming g n = g.Conns |> List.filter (fun c-> c.On && c.To=n) |> List.map (fun x->x.From)
    let outgoing g n = g.Conns |> List.filter (fun c-> c.On && c.From=n) |> List.map (fun x->x.To)

    ///remove dead nodes
    ///only keep nodes that are on the path 
    ///from inputs to outputs                   
    let tryTrimGraph (g:Graph) =
        match tryValidate(g) with 
        | Choice2Of2 ex -> Choice2Of2 ex
        | Choice1Of2 _ -> 
            let outputs = g.Nodes |> Map.toSeq |> Seq.filter (snd>>isOutput) |> Seq.map fst |> Seq.toList
            let inputs = g.Nodes |> Map.toSeq |> Seq.filter (snd>>isInput) |> Seq.map fst |> Seq.toList

            let rec loop isTerminal getLinks (vstd:Set<Id>) ls =
                match ls with
                | [] -> vstd
                | (n::rest)::remain when isTerminal g.Nodes.[n] && vstd.Contains n |> not ->
                    let vstd' = Set.add n vstd
                    loop isTerminal getLinks vstd' (rest::remain)
                | (n::rest)::remain when vstd.Contains n |> not ->
                    let vstd' = Set.add n vstd
                    let linkedNodes = getLinks g n
                    loop isTerminal getLinks vstd' (linkedNodes::rest::remain)
                | (_::rest)::remain -> loop isTerminal getLinks vstd (rest::remain)
                | []::remain ->  loop isTerminal getLinks vstd remain

            let reachableByOutput = loop isInput incoming Set.empty [outputs]
            let reachableByInputs = loop isOutput outgoing Set.empty [inputs]
            let reachable = Set.intersect reachableByInputs reachableByOutput
            let nodes = g.Nodes |> Map.filter (fun id _ -> reachable.Contains id)
            let conns = g.Conns |> List.filter (fun c->c.On && reachable.Contains c.From && reachable.Contains c.To)
            let g' = {Nodes=nodes; Conns=conns}                                                                                                    
            match tryValidate(g') with 
            | Choice2Of2 ex -> Choice2Of2 ex
            | Choice1Of2 _  -> Choice1Of2 g'

    let trimGraph g = match tryTrimGraph g with Choice1Of2 g' -> g' | Choice2Of2 ex -> failwith ex

    let distCont a b = abs ((float a) - (float b)) / (float (a + b))

    ///a measure of distance between two dense cell types
    let distDense (d1:Dense) (d2:Dense) =
        [
            (if (d1.Activation = d2.Activation) then 0.0 else 1.0)
            (if (d1.Bias = d2.Bias) then 0.0 else 1.0)
            distCont d1.Dims d2.Dims
        ]
        |> List.average

    ///a measure of distance between two conv2d cell types
    let distConv2d (d1:Conv2D) (d2:Conv2D) =
        [
            (if (d1.Activation = d2.Activation) then 0.0 else 1.0)
            distCont d1.Filters d2.Filters
            distCont d1.Stride d2.Stride
            distCont d1.Kernel d2.Kernel
        ]
        |> List.average

    ///a measure of distance between two cells
    let distCell (c1:CellType) (c2:CellType) =
        let W = 1.0
        match c1, c2 with
        | ModuleSpecies a, ModuleSpecies b when a = b -> 0.0
        | Norm BatchNorm, Norm BatchNorm              -> 0.0
        | Norm LayerNorm, Norm LayerNorm              -> 0.0
        | Dense d1, Dense d2                          -> distDense d1 d2
        | Conv2D d1, Conv2D d2                        -> distConv2d d1 d2
        | SubGraph s1, SubGraph s2                    -> distGraph s1 s2
        | _, _                                        -> W
       
    ///a measure of distance between two nodes
    let distConn (n1:Node) (n2:Node) =
        let W=2.0
        match n1.Type, n2.Type with
        | Input _, Input _      -> 0.0
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

    ///trim the main graph and any subgraphs
    ///contained within 
    let compress (g:Graph) =
        let nodes = g.Nodes |> Map.map (fun id sg -> 
            let nt = match sg.Type with Cell (SubGraph g) -> trimGraph g |> SubGraph |> Cell | n -> n
            {sg with Type = nt})
        {g with Nodes=nodes} |> trimGraph
