namespace GELang

    //language 'grammar'
    module rec G =
        type Node = {Id:Id; Type:NodeType}
        type Id = Id of string
        type NodeType = Cell of Cell | Output of Dense | Input
        type Conn = {On:bool; From:Id; To:Id; Innovation:int}
        type Dense = {Dims:int; Bias:bool; Activation:Activation}
        type Cell = {PreBN:bool; Op:Operation; PostBN:bool}
        type Operation = Embed of int  | Dense of Dense | OpMax | OpMin | OpAbs 
        type Activation = NONE | Elu | Relu | LeakyRelu | Sig
        type Graph = {Nodes:Map<Id,Node>; Conns:Conn list}

    //non-blocking id generation
    [<AutoOpen; RequireQualifiedAccess>]
    module IdGen =
        let mutable private nodeId = 0
        let mutable private connId = 0
        let node() = let i = System.Threading.Interlocked.Increment(&nodeId) in string i
        let conn() = let i = System.Threading.Interlocked.Increment(&connId) in i

    //operations on graphs  - including graph evolution
    module GOps =
        open G
        open Probability
        open System.Collections.Generic

        type Cfg =
            {
                MaxNodes : int
                MinCellDims : int
                MaxCellDims : int
            }

        type Match = SameL | DiffL | FrmL | FrmR| ExsL | ExsR

        module private GOpsCore =

            let isInput (n:Node) = match n.Type with Input  -> true | _ -> false
            let isOutput (n:Node) = match n.Type with Output _ -> true | _ -> false

            let randActivation (ex:Activation option) : Activation =
                    let activations = FSharp.Reflection.FSharpType.GetUnionCases(typeof<Activation>)
                    let exVal = ex |> Option.map (fun a-> FSharp.Reflection.FSharpValue.GetUnionFields(a,typeof<Activation>) |> fst) 
                    let activations = exVal |> Option.map (fun x -> activations |> Array.filter (fun y-> x=y |> not)) |> Option.defaultValue activations
                    let act = activations.[RNG.Value.Next(activations.Length)]
                    FSharp.Reflection.FSharpValue.MakeUnion(act,[||]) :?> _

            ///generate a new random cell
            let genCell cfg =
                    let dims = RNG.Value.Next(cfg.MinCellDims,cfg.MaxCellDims)
                    let preBN = RNG.Value.NextDouble() > 0.5
                    let postBN = not preBN
                    let bias = RNG.Value.NextDouble() > 0.25
                    let activation = randActivation None
                    {
                      Id = IdGen.node() |> Id
                      Type =               
                              Cell  {
                                          PreBN = preBN
                                          Op = Dense {Dims = dims; Bias=bias; Activation=activation}
                                          PostBN = postBN
                                    }
                    }

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

            let rng = System.Random()
            ///get a randomly selected connection
            let randConn (g:Graph) = 
                g.Conns.[Probability.RNG.Value.Next(g.Conns.Length)]
                //g.Conns.[rng.Next(g.Conns.Length)]

            ///update the connection list by removing one connection and adding a list of new connections
            let updateConns removeConn addList baseList = 
                baseList
                |> List.filter (fun c -> c <> removeConn)
                |> List.append addList
                |> List.sortBy (fun c->c.Innovation)

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
                    | [],r::rest                                         -> loop ((ExsR,r)::acc) ls rest
                    | l::rest,[]                                         -> loop ((ExsL,l)::acc) rest rs
                    | l::restL,r::restR when l = r                       -> loop ((SameL,l)::acc) restL restR
                    | l::restL,r::restR when l.Innovation = r.Innovation -> loop ((DiffL,l)::acc) restL restR
                    | l::restL,r::restR when l.Innovation < r.Innovation -> loop ((FrmL,l)::acc) restL rs
                    | _,r::restR                                         -> loop ((FrmR,r)::acc) ls restR
        
                loop [] acs bcs
        
        open GOpsCore

        ///merge genes of two parents
        ///parent a is considered dominant (fitter)
        let crossover cfg (a:Graph) (b:Graph) =
            let ms = diffConn a b
            let nodes,conns = (([],[]),ms) ||> List.fold (fun (ns,cs) mtch -> 
                match mtch with
                | SameL,c | DiffL,c | FrmL,c | ExsL,c -> a.Nodes.[c.From]::a.Nodes.[c.To]::ns,c::cs
                | FrmR,c | ExsR, c -> b.Nodes.[c.From]::b.Nodes.[c.To]::ns,c::cs
                )
            {Nodes=nodes |> List.map (fun n->n.Id,n) |> Map.ofList; Conns=conns}

        ///add node to graph by splitting a connection
        //('complexify' in NEAT)
        let addNode cfg (g:Graph) =
            let conn = randConn g
            let newNode = genCell cfg 
            let forwardConn = {On=true; Innovation=IdGen.conn(); From=newNode.Id; To=conn.To}
            let backConn = {conn with To=newNode.Id; Innovation=IdGen.conn()}
            let disConn = {conn with On=false}
            let conns = g.Conns |> updateConns conn [backConn;forwardConn;disConn] 
            {Nodes=g.Nodes |> Map.add newNode.Id newNode; Conns=conns}

        ///toggle a random connection
        let toggleConnection cfg (g:Graph) =
            let oldConn = randConn g
            let newConn = {oldConn with On=not oldConn.On}
            {g with Conns = g.Conns |> updateConns oldConn [newConn]}

        ///add a random connection
        let addConnection cfg (g:Graph) =
           match randUnconn g with 
           | Some (f,t) -> {g with Conns = {On=true; Innovation=IdGen.conn(); From=f; To=t}::g.Conns}
           | None       -> g

        /// modify some random parameter of node to be like that of the template node
        let mimicParm cfg (g:Graph) nodeId tmpltNodeId =
            let na = g.Nodes.[nodeId]
            let nb = g.Nodes.[tmpltNodeId]
            let nType =
                match na.Type,nb.Type with
                | Cell ca, Cell cb -> 
                    match RNG.Value.Next(3) with
                    | 0 -> {ca with PreBN=cb.PreBN}
                    | 1 -> {ca with PostBN=cb.PostBN}
                    | _ -> {ca with Op=cb.Op}
                    |> Cell
                | x,_ -> x
            {g with Nodes=g.Nodes |> Map.add nodeId {na with Type=nType}}

        ///mutate some random parameter of the node
        let evolveParm cfg (g:Graph) nodeId  =
            let na = g.Nodes.[nodeId]
            let nType =
                match na.Type with
                | Cell ca -> 
                    match RNG.Value.Next(4) with
                    | 0 -> {ca with PreBN=not ca.PreBN}
                    | 1 -> {ca with PostBN=not ca.PostBN}
                    | 2 -> 
                        match ca.Op with
                        | Dense d -> {ca with Op=Dense {d with Dims=RNG.Value.Next(cfg.MinCellDims,cfg.MaxCellDims)}}
                        | _ -> ca
                    | _ -> 
                        match ca.Op with
                        | Dense d -> {ca with Op=Dense {d with Activation=randActivation (Some d.Activation)}}
                        | _ -> ca
                    |> Cell
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
            {Nodes=trimNodes; Conns=trimConns}
        










            
        
        

