namespace CATNeuro
open Ext
open FSharp.Reflection
open Probability

module rec NormativeKS =         
    let acceptance ca cfg species (st,topG) =
        let nmst = st.NmState
        let nmst' = updateState nmst topG
        let st' = {st with NmState=nmst'}
        (st',topG)

    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = 
        let nmst = st.NmState
        let indvs' = indvs |> Array.map (influenceIndv nmst)
        st,indvs'

    let updateState st topP =
        //new high perf indvs
        let highPerf = 
            Array.append topP st.TopIndv 
            |> Array.sortBy (fun x -> x.Fitness.[0])
            |> Array.truncate st.MaxIndv

        //extract norms by innovation #
        let norms =
            highPerf
            |> Seq.collect (fun indv -> indv.Graph.Conns 
                                        |> List.map (fun c->c.Innovation, indv.Graph.Nodes.[c.To].Type))
            |> Seq.choose (fun (i,n) -> match n with Cell c -> Some (i,c) | _ -> None)
            |> Seq.collect (fun (i,n) -> nodeParms n |> List.map (fun (p,d) -> (i,p),d))
            |> Seq.groupBy fst
            |> Seq.map (fun ((i,p),xs) -> i,p, xs |> Seq.map snd |> Seq.toList |> aggregateParms)
            |> Seq.choose (fun (i,p,agg) -> agg |> Option.map(fun agg -> i,(p,agg)))
            |> Seq.groupBy fst
            |> Seq.map (fun (i,xs) -> i, xs |> Seq.map snd |> Map.ofSeq)            
            |> Map.ofSeq
        {st with Norms=norms}

    let mass        = function Density fs  -> fs  | _ -> failwith "density expected"
    let caseWheel   = function Cases whl   -> whl | _ -> failwith "cases expected"
    let classWheel  = function Classes whl -> whl | _ -> failwith "classes expected"

    let reify<'a> v = FSharpValue.MakeUnion(v,[||]) :?> 'a

    let updateNode n (pm:Map<ParmType,Dist>) =
        let ty =
            match n.Type with
            | Cell (Dense d) -> 
                let dims = pm |> Map.tryFind PDims |> Option.map mass
                let acts = pm |> Map.tryFind PActivation |> Option.map caseWheel
                let bias = pm |> Map.tryFind PBias |> Option.map caseWheel
                let d = dims |> Option.fold (fun d fs -> {d with Dims=int fs.[0]}) d
                let d = acts |> Option.fold (fun d whl -> {d with Activation = spinWheel whl |> reify} ) d
                let d = bias |> Option.fold (fun d whl -> {d with Bias=spinWheel whl |> reify}) d
                Dense d
            | Cell (ModuleSpecies s ) -> 
                pm 
                |> Map.tryFind PSpecies 
                |> Option.map classWheel
                |> Option.fold (fun m w -> spinWheel w |> ModuleSpecies) (ModuleSpecies s)
            | Cell (Norm nt) ->
                pm 
                |> Map.tryFind PNorm 
                |> Option.map caseWheel
                |> Option.fold (fun n w -> spinWheel w |> reify |> Norm) (Norm nt)
            | x -> failwithf "case not handled %A" x
        {n with Type = Cell ty}

    let influenceIndv nmst indv =
        let nodes' = 
            indv.Graph.Conns
            |> Seq.map (fun c -> c.Innovation, indv.Graph.Nodes.[c.To])
            |> Seq.choose (fun (i,n) -> nmst.Norms |> Map.tryFind i |> Option.map (fun pm->i,n,pm))
            |> Seq.map (fun (i,n,pm) -> updateNode n pm)
        let gNodes = (indv.Graph.Nodes,nodes') ||> Seq.fold (fun acc n -> acc |> Map.add n.Id n)
        let g = {indv.Graph with Nodes=gNodes}
        {indv with Graph=g}

    let nodeParms = function
        | Dense d -> 
            [
                PDims       , Cont (float d.Dims)
                PActivation , Case (FSharpValue.GetUnionFields(d.Activation,typeof<Activation>) |> fst) 
                PBias       , Case (FSharpValue.GetUnionFields(d.Bias,typeof<Bias>) |> fst)
            ]
        | ModuleSpecies s ->
            [
                PSpecies    , Class s
            ]
        | Norm l ->
            [
                PNorm       , Case (FSharpValue.GetUnionFields(l,typeof<NormalizationType>) |> fst)
            ]
        | _      -> printfn "unexpected node type"; []

    let cases xs   = xs |> List.map (function Case u  -> u | _ -> failwith "not expected")
    let classes xs = xs |> List.map (function Class c -> c | _ -> failwith "not expected")
    let conts xs   = xs |> List.map (function Cont f  -> f | _ -> failwith "not expected")

    let aggregateParms xs =
        match xs with
        | []            ->  None
        | Case _::_     ->  xs 
                            |> cases 
                            |> List.countBy yourself 
                            |> List.map (fun (x,c)->x,float c) 
                            |> List.toArray
                            |> createWheel 
                            |> snd
                            |> Cases |> Some
        | Cont _::_     ->  xs 
                            |> conts 
                            |> List.toArray 
                            |> Density |> Some
        | Class _::_    ->  xs 
                            |> classes
                            |> List.countBy yourself
                            |> List.map (fun (x,c)->x,float c)
                            |> List.toArray
                            |> createWheel
                            |> snd
                            |> Classes |> Some
                

(*
Normative understands ranges and distributions.
Tracks these for best performers
Move paramters of influenced indiviuals towards the ranges of best

Parameter types:
- global parameters e.g. learning rate
- cell type
    | Cell (ModuleSpecies a) -> pick from distribution
    | Cell (Dense a)         -> pick from distrbiution of dims, bias, activation
    | Cell (Norm a)          -> pick from distribution between two types

*)


