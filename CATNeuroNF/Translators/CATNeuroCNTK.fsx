//Script for tranlating (abstract) CATNeuro graphs to (concrete) CNTK models
#load @"..\Tests\CNSetEnv.fsx"

#I @"C:\s\Repos\"                                //repo directory  - set for your environment
                                                 //external source references are based on this

#load @"FsCNTK\FsCNTK\Scripts\FsCNTK_SetEnv.fsx" //load FsCNTK source
#load @"FsCNTKTools\FsCNTKTools\Graph.fs"        //load FsCNTK tools for graph drawing

module rec CAT_CNTK_Types =
    open CATNeuro

    type CATNode = CATNeuro.Node
    type CNTKNode = FsCNTK.Node
    type GenCtx = {Graph:CnGraph; Parent:(CnGraph*Id) option; Cnctr:Connector option}

    type DimResolve = Infer | Fixed of int
    type Connector = {Inp:int option; Out:DimResolve}
    type CnNode =  Id * CnNodeType
    type CnNodeType =
        | CnInp     of (string*Connector*CNTKNode)
        | CnFunc    of (Connector*(CNTKNode->CNTKNode)) 
        | CnOut     of (Connector*(CNTKNode->CNTKNode))
        | CnModule  of CnGraph
        | CnLinkIn 
        | CnLinkOut

    and CnGraph = {CnNodes:Map<Id,CnNode>; CnConns:Conn list}

    type CnAcc = {G:Graph; Resolver:Map<Id,CnNode>; Inputs:Map<string,CNTKNode>}
    type GenAcc = {Graph:CnGraph;  Cnctr:Connector option; Resolution:Map<Id,ResolvedNode>; SubgraphInput:ResolvedNode option}

    type ResolvedNode = {DimDown:DimResolve; N:CNTKNode}

module CAT_CNTK_Utils =
    open CAT_CNTK_Types
    open CATNeuro
    type NodeM = Microsoft.Msagl.Drawing.Node
    let ROOT = "root"

    let toDim = function  Infer -> " - " | Fixed f -> string f 

    let showGraph title (m:FsCNTK.Node) = FsCNTK.Tools.ModelViz.showGraphT title FsCNTK.Tools.Expand.NoExpansion m.Func

    type Nd = S of string*string | N of string*string
    type Nt = {Nid:Nd; T:CnNodeType}
    type Edg = {F:string*string; Innov:int; On:bool; T:string*string}

    type Shp = Microsoft.Msagl.Drawing.Shape
    type Stl = Microsoft.Msagl.Drawing.Style

    let styleNode (n:Microsoft.Msagl.Drawing.Node) =
        let nt = n.UserData :?> CnNodeType
        match nt with
        | CnInp  (l,cn,_) -> n.Attr.Shape <- Shp.Ellipse
                             n.LabelText <- sprintf "%s [%s]" l (toDim cn.Out)
        | CnFunc (cn,_)   -> n.Attr.Shape <- Shp.Box
                             n.LabelText <- sprintf "%s [%s]" n.LabelText (toDim cn.Out)
        | CnOut  (cn,_)   -> n.Attr.Shape <- Shp.DoubleCircle
                             n.LabelText <- sprintf "%s [%s]" n.LabelText (toDim cn.Out)
        | CnLinkIn        -> n.Attr.Shape <- Shp.Ellipse
        | CnLinkOut       -> n.Attr.Shape <- Shp.DoubleCircle
        | CnModule  cng   ->  ()

    let styleEdge (e:Microsoft.Msagl.Drawing.Edge) =
        let tE = e.UserData :?> bool
        if not tE then e.Attr.AddStyle Stl.Dotted

    let rec collectNodes acc graphId (g:CnGraph) =
        let sgId = graphId |> Option.map(fun x->x) |> Option.defaultValue ROOT
        let acc' =
            (acc,g.CnNodes |> Map.toList)
            ||> List.fold (fun (accN,accE) (Id s,(_,nt)) ->
                let nId = s
                match nt with
                | CnInp  (_,cn,_) //of (string*Connector*CNTKNode)
                | CnFunc (cn,_) //of (Connector*(CNTKNode->CNTKNode)) 
                | CnOut  (cn,_) //of (Connector*(CNTKNode->CNTKNode))
                             -> {Nid=N(sgId,nId);T=nt}::accN,accE
                | CnLinkIn 
                | CnLinkOut  -> {Nid=N(sgId,nId);T=nt}::accN,accE
                | CnModule  cng -> 
                    let accN'= {Nid=S(sgId,nId);T=nt}::accN
                    collectNodes (accN',accE) (Some s) cng)

        (acc',g.CnConns)
        ||> List.fold (fun (accN,accE) c -> 
            let (Id f) = c.From
            let (Id t) = c.To
            let f = (sgId,f)
            let t = (sgId,t)
            accN,{F=f;Innov=c.Innovation; On=c.On; T=t}::accE
        )

    let groupId n = n.Nid |> function N (g,_) | S(g,_) -> g
    let nodeId n = n.Nid |> function N (_,n) | S(_,n) -> n
    let isSg n = n.Nid |> function S(_,_) -> true | _ -> false
    let nid = function S(a,b) | N(a,b) -> a + "_" + b
    let ntid x = x.Nid
    
    let makeGraph (g:CnGraph)  =
        let gr = new Microsoft.Msagl.Drawing.Graph()
        let rootGr = Microsoft.Msagl.Drawing.Subgraph(ROOT)
        gr.RootSubgraph <- rootGr
        
        let (nodes,edges) = collectNodes ([],[]) None g
        let nodesG = nodes |> List.groupBy groupId

        let root = nodesG |> List.find (fun (k,_) -> k=ROOT)
        let sgs = nodesG |> List.filter (fun (k,_)->k<>ROOT)

        let addNode x = 
            let id = ntid x |> nid 
            let n = gr.AddNode(id)
            n.LabelText <- nodeId x
            n.UserData <- x.T
            n

        let rootNodes = root |> snd |> List.filter (isSg>>not) |> List.map addNode
        let sgsNodes = 
            sgs 
            |> List.map (fun (k,vs) -> 
                k,
                vs |> List.filter (isSg>>not) |> List.map addNode)

        rootNodes |> List.iter (rootGr.AddNode)

        let sgsGrs =
            sgsNodes
            |> List.map (fun (k,vs) ->
                let sg = Microsoft.Msagl.Drawing.Subgraph(S(ROOT,k) |> nid)
                sg.LabelText <- k
                vs |> List.iter (sg.AddNode)
                sg)

        sgsGrs |> List.iter rootGr.AddSubgraph
        let es = 
            edges 
            |> List.map (fun edg -> 
                let e = gr.AddEdge(N edg.F |> nid,string edg.Innov,N edg.T |> nid)
                e.UserData <- edg.On
                e)

        gr.Nodes |> Seq.iter styleNode
        es |> Seq.iter styleEdge
        gr

    let showCnGraph title (g:CnGraph) =
        let g = makeGraph g
        CNSetEnv.visGraph title g

module CATNeuroCNTK =

    open FsCNTK
    open Probability
    open CAT_CNTK_Types
    open CATNeuro

    let isInferred = function Infer -> true | _ -> false
    let isFixed = isInferred>>not

    let cnActivation = function
        | CATNeuro.NONE -> FsCNTK.NONE
        | Elu           -> FsCNTK.ELU
        | Relu          -> FsCNTK.ReLU
        | LeakyRelu     -> FsCNTK.LeakyReLU None
        | Sig           -> FsCNTK.Sigmoid
        | TanH          -> FsCNTK.Tanh

    let volume (n:CNTKNode) = dims n.Shape |> List.reduce ( * )

    let cnBias = function   
        | Bias.On -> true
        | Bias.Off -> false
     
    let cnInput name node = 
        CnInp (name,{Inp=None; Out=Fixed (volume node)},node)

    let accUpd acc id cn = {acc with Resolver = acc.Resolver |> Map.add id (id,cn)}

    let rec translateGraph inputMap (g:Graph) =
        let acc = {Resolver=Map.empty; G=g; Inputs=inputMap}                                            
        let acc' = 
            g.Nodes 
            |> Map.toSeq 
            |> Seq.map snd 
            |> Seq.fold translateNode acc
        {CnNodes=acc'.Resolver; CnConns=g.Conns}

    and translateNode acc (nd:CATNode) =
        match nd.Type with
        | Input n           -> accNamedInput acc nd.Id n  //blueprint input
        | ModInput          -> accInLink acc nd.Id
        | ModOutput         -> accOutLink acc nd.Id
        | Cell (Dense d)    -> accDense acc nd.Id d
        | Cell (Norm normT) -> accNorm acc nd.Id normT
        | Output d          -> accOutput acc nd.Id d
        | Cell (SubGraph g) -> accSubGraph acc nd.Id g
        | x                 -> failwithf "unexpected cell type encountered in translation %A" x

    and accSubGraph acc ndid g =
        let cn = translateGraph Map.empty g |> CnModule
        accUpd acc ndid cn
   
    and accInLink acc ndid = accUpd acc ndid CnLinkIn
    and accOutLink acc ndid = accUpd acc ndid CnLinkOut 

    and accDense acc ndid d = 
        let layer  = L.Dense(
                        D d.Dims, 
                        activation=cnActivation d.Activation,
                        bias = cnBias d.Bias
                        )
        let cn = CnFunc ({Inp=None;Out=Fixed d.Dims},layer)
        accUpd acc ndid cn

    and accOutput acc ndid d = 
        let layer  = L.Dense(
                        D d.Dims, 
                        activation=cnActivation d.Activation,
                        bias = cnBias d.Bias
                        )
        let cn = CnOut ({Inp=None;Out=Fixed d.Dims},layer)
        accUpd acc ndid cn
    
    and accNorm acc ndid d = 
        let layer  = match d with  BatchNorm -> L.BN() | LayerNorm -> L.LayerNormalization()
        let cn = CnFunc ({Inp=None;Out=Infer},layer)
        accUpd acc ndid cn

    and accNamedInput acc ndid name =
        let (_,cn) = 
            acc.Resolver
            |> Map.tryFind ndid
            |> Option.defaultValue (
                acc.Inputs |> Map.tryFind name
                |> Option.map (fun node -> (ndid,cnInput name node))
                |> Option.defaultWith(fun()->failwithf "no match for input '%s'" name))
        accUpd acc ndid cn

    let isOut = function CnOut _ | CnLinkOut -> true  | _ -> false
    let isInput = function CnInp _ -> true | _ -> false

    let preds (ctx:GenAcc) id = ctx.Graph.CnConns |> List.filter (fun c->c.To=id)
    let toNode (ctx:GenAcc) conn = ctx.Graph.CnNodes.[conn.To]
    let fromNode (ctx:GenAcc) conn = ctx.Graph.CnNodes.[conn.From]

    let inferDims c = 
        match c with 
        | None -> Infer //failwith "expect some output dimension"
        | Some c ->
            match c.Out with
            | Infer     -> Infer //failwith "cannot infer dimensions"
            | Fixed d   -> Fixed d

    let combine (ctx:GenAcc) ls = 
        match ls with
        | x::[] -> x
        | ls ->
            let ns = ls |> List.map (fun n->n.N)
            let ndims = ns |> List.map (O.shape) |> List.map dims |> List.map (List.fold ( * ) 1) 
            let dimSum = ndims |> Seq.map int64 |> Seq.sum 
            if dimSum <= 200L then        
                let spliced = O.splice ns
                let dimS = volume spliced
                //printfn "Spliced: graph  nodes %d | total dims = %d" ctx.Graph.CnNodes.Count dimS
                {DimDown=Fixed dimS; N=spliced}
            else
                let maxDim = ndims |> Seq.max
                let ns' = 
                    List.zip ndims ns  
                    |> List.map (fun (d,n)-> 
                        if d = maxDim then 
                            n 
                        else
                            let df = (maxDim - d) / 2
                            let extra = maxDim - ((df * 2) + d)
                            let n' = O.pad(n, [(df,df+extra)])
                            //printfn "%A - %A" (n |> O.shape |> dims) (n' |> O.shape |> dims)
                            n'
                            )
                let combined = O.sum ns'
                //printfn "Summed: graph  nodes %d | total dims = %d" ctx.Graph.CnNodes.Count maxDim
                {DimDown=Fixed maxDim; N=combined}

    let rec accIncoming acc incoming = incoming |> List.map (fromNode acc) |> List.fold bindNode acc 

    and addNode id n acc = {acc with Resolution=acc.Resolution |> Map.add id n}

    and resolveIncoming acc id =
        let incoming = preds acc id
        let acc' = accIncoming acc incoming 
        let inputNodes = incoming |> List.map (fromNode acc')
        let resolvedInputs = inputNodes |> List.map (fun (i,_) -> acc'.Resolution.[i])
        let inp = combine acc' resolvedInputs
        acc',inp

    and convertNode acc (id,cn:CnNodeType) = 
        match cn with
        | CnInp (_,cn,n) -> {acc with Resolution= acc.Resolution |> Map.add id {DimDown=cn.Out; N=n}}

        | CnFunc(c,fn)  -> let c' = match c.Out with Infer -> {c with Out=inferDims acc.Cnctr} | _ -> c
                           let acc' = {acc with Cnctr=Some c' }
                           let acc'',inp = resolveIncoming acc' id
                           let dim' = match c.Out with Fixed d -> Fixed d | _ -> inp.DimDown
                           let n = fn inp.N
                           let dr = {DimDown=dim'; N=n}
                           acc'' |> addNode id dr

        | CnModule  gr  -> let acc',inp = resolveIncoming acc id
                           let out = genSubModel inp gr
                           acc' |> addNode id  out

        | CnLinkOut     -> let acc',inp = resolveIncoming acc id
                           acc' |> addNode id  inp

        | CnOut(c,fn)   ->  let c' = match c.Out with Infer -> {c with Out=inferDims acc.Cnctr} | _ -> c
                            let acc' = {acc with Cnctr=Some c' }
                            let acc'',inp = resolveIncoming acc' id
                            let dim' = match c.Out with Fixed d -> Fixed d | _ -> inp.DimDown
                            let n = fn inp.N
                            let dr = {DimDown=dim'; N=n}
                            acc'' |> addNode id dr

        | CnLinkIn      ->  match acc.SubgraphInput with
                            | Some n -> acc |> addNode id n
                            | None -> failwith "no resolved input node for subgraph"    


    and bindNode acc (id,cn:CnNodeType) =
        match acc.Resolution |> Map.tryFind id with
        | Some n -> acc
        | None  -> convertNode acc (id,cn)

    and genSubModel (inp:ResolvedNode) gr =
        let acc = {Graph=gr; Cnctr=None; Resolution=Map.empty; SubgraphInput=Some inp}
        let (id,cn) = gr.CnNodes |> Map.toSeq |> Seq.find (snd>>snd>>isOut)
        let acc' =
            [cn]
            |> Seq.fold bindNode acc
        acc'.Resolution.[id]

    and genModel outputNode (acc:GenAcc) =
        let acc' = 
            [outputNode]
            |> Seq.fold bindNode acc
        acc'

    let translateAssembly inputMap (n:NetworkAssembly) =
        let cng = translateGraph inputMap n.Model
        let (id,cn) = cng.CnNodes |> Map.toSeq |> Seq.find (snd>>snd>>isOut)
        let acc = {Graph=cng; Resolution=Map.empty; Cnctr=None; SubgraphInput=None}
        let acc' = genModel cn acc
        acc'.Resolution.[id].N



