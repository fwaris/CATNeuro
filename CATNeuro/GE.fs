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

        module private GOpsCore =

            module IdGen =
                let mutable private nodeId = 0
                let mutable private connId = 0
                let node() = let i = System.Threading.Interlocked.Increment(&nodeId) in string i
                let conn() = let i = System.Threading.Interlocked.Increment(&connId) in i

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

            ///topologically sort grpah
            let tsort (g:Graph) =
                let adjM = g.Conns |> List.groupBy (fun c->c.From) |> Map.ofList
                let dgriM = adjM |> Map.map (fun k v -> v.Length)
            
                //(int*node) set used as priority q (credit: John Harrop)
                let q = dgriM |> Seq.map (fun kv -> kv.Value,kv.Key) |> set 
   
                let rec loop acc m q =
                    if Set.isEmpty q then 
                        acc |> List.rev
                    else
                        let (_,id) as minE = q |> Set.minElement 
                        let conns = adjM.[id]
                        let q = q |> Set.remove minE
                        let (m,q) = ((m,q),conns) ||> List.fold (fun (m,q) c -> 
                            let inDegree = m |> Map.find c.To
                            let q = q |> Set.remove (inDegree,c.To)
                            let inDegree = inDegree - 1
                            let m = m |> Map.add id inDegree
                            let q =
                                if inDegree >= 0 then
                                    q |> Set.add (inDegree,c.To)
                                else
                                    q
                            (m,q))
                        loop (id::acc) m q
                loop [] dgriM q

            ///get a randomly selected connection
            let randConn (g:Graph) = g.Conns.[Probability.RNG.Value.Next(g.Conns.Length)]

            ///update the connection list by removing one connection and adding a list of new connections
            let updateConns removeConn addList baseList = 
                baseList
                |> List.filter (fun c -> c <> removeConn)
                |> List.append addList
                |> List.sortBy (fun c->c.Innovation)

            let isInput (n:Node) = match n.Type with Input  -> true | _ -> false
            let isOutput (n:Node) = match n.Type with Output _ -> true | _ -> false

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

        open GOpsCore

        ///add node to graph by splitting a connection
        //('complexify' in NEAT)
        let addNode cfg (g:Graph) =
            let conn = randConn g
            let newNode = genCell cfg 
            let forwardConn = {On=true; Innovation=IdGen.conn(); From=newNode.Id; To=conn.To}
            let backConn = {conn with To=newNode.Id}
            let conns = g.Conns |> updateConns conn [backConn;forwardConn] 
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
        
        let printConn c = 
            let (Id f) = c.From
            let (Id t) = c.To
            let on = if c.On then "->" else "--"
            sprintf "%d.%s%s%s" c.Innovation f on t











            
        
        

