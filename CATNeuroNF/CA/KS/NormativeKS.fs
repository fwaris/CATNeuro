namespace CATNeuro
open Ext
open FSharp.Reflection
open Probability

module rec NormativeKS =         
    let acceptance ca cfg species (st,topG) =
        let nmst = st.NmState
        let nmst' = updateState ca cfg nmst topG
        let st' = {st with NmState=nmst'}
        (st',topG)

    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = 
        let nmst = st.NmState
        let indvs' = indvs |> Array.map (influenceIndv cfg nmst)
        st,indvs'

    let influenceIndv cfg nmst indv =
        let nodes' = 
            indv.Graph.Conns
            |> Seq.map (fun c -> c.Innovation, indv.Graph.Nodes.[c.To])
            |> Seq.choose (fun (innovNum,n) -> nmst.Norms |> Map.tryFind innovNum |> Option.map (fun pm->innovNum,n,pm))
            |> Seq.map (fun (i,n,pm) -> updateNode cfg n pm)
        let gNodes = (indv.Graph.Nodes,nodes') ||> Seq.fold (fun acc n -> acc |> Map.add n.Id n)
        let g = {indv.Graph with Nodes=gNodes}
        {indv with Graph=g}

    let updateState ca cfg st (topP:Individual[]) =
        //new high perf indvs
        let highPerf = 
            Array.append topP st.TopIndv 
            |> CAUtils.rankIndvs ca 
            |> Array.truncate st.MaxIndv

        //extract norms by innovation #
        let norms =
            highPerf
            |> Seq.collect (fun indv -> indv.Graph.Conns 
                                        |> List.map (fun c->c.Innovation, indv.Graph.Nodes.[c.To].Type))  //innov#, nodeType
            |> Seq.choose (fun (i,n) -> match n with Cell c -> Some (i,c) | _ -> None)                    //cells
            |> Seq.collect (fun (i,n) -> nodeParms cfg n |> List.map (fun (p,d) -> (i,p),d))              //extract parms from cells
            |> Seq.groupBy fst                                                                            //group by innov#
            |> Seq.map (fun ((i,p),xs) -> i,p, xs |> Seq.map snd |> Seq.toList |> aggregateParms)         //aggregate group to get parm distributions
            |> Seq.choose (fun (i,p,agg) -> agg |> Option.map(fun agg -> i,(p,agg)))                      //filter None
            |> Seq.groupBy fst                                                                            //re-group by innov# 
            |> Seq.map (fun (i,xs) -> i, xs |> Seq.map snd |> Map.ofSeq)                                  //innov# * (parmType->distribution) map
            |> Map.ofSeq                                                                                  //map with innov# as key
        {st with Norms=norms}

    let mass        = function Density fs  -> fs  | _ -> failwith "density expected"
    let caseWheel   = function Cases whl   -> whl | _ -> failwith "cases expected"
    let classWheel  = function Classes whl -> whl | _ -> failwith "classes expected"

    let reify<'a> v = FSharpValue.MakeUnion(v,[||]) :?> 'a

    ///update parameters of the given node by following the 'norms' 
    ///of the best indviduals in the population
    ///each node type and all of its 'named' parameters are considered here
    let updateNode cfg n (pm:Map<ParmType,Dist>) : Node =
        let ty =
            match n.Type with
            | Cell (Dense d) -> 
                let dims = 
                    pm 
                    |> Map.tryFind PDims 
                    |> Option.map mass  
                    |> Option.bind (fun xs->if xs.Length >= 2 then Some(xs) else None) //don't use distribution if only 1 point in set
                    |> Option.map (CAUtils.sampleDensity 3.0 >> int)   //sample from kernel density estimate
                    |> Option.defaultValue (GraphOps.randDims cfg)     //sample from uniform, if None

                let acts = 
                    pm 
                    |> Map.tryFind PActivation 
                    |> Option.map caseWheel
                    |> Option.map (spinWheel >> reify)                       //sample from dist
                    |> Option.defaultValue (GraphOps.randActivation None)    //random, if None


                let bias = 
                    pm 
                    |> Map.tryFind PBias 
                    |> Option.map caseWheel
                    |> Option.map (spinWheel >> reify)                      //sample from dist
                    |> Option.defaultValue (GraphOps.randBias())            //random, if None

                let d' = {d with Dims=dims; Activation=acts; Bias=bias}
                Dense d'

            | Cell (ModuleSpecies s ) -> 
                pm 
                |> Map.tryFind PSpecies 
                |> Option.map classWheel
                |> Option.map (fun w -> spinWheel w |> ModuleSpecies)                            //sample from dist
                |> Option.defaultValue (cfg.NumSpecies |> CAUtils.randSpecies |> ModuleSpecies)  //random, if None

            | Cell (Norm nt) ->
                pm 
                |> Map.tryFind PNorm 
                |> Option.map caseWheel
                |> Option.map (fun w -> spinWheel w |> reify |> Norm)                            //sample from dist
                |> Option.defaultValue (GraphOps.randNormalization (Some nt) |> Norm)            //random, if None

            | x -> failwithf "case not handled %A" x
        {n with Type = Cell ty}

    ///pattern match cell type to 
    ///extract named parameters
    let nodeParms cfg = function
        | Dense d -> 
            [
                PDims       , Cont (float d.Dims)
                PActivation , Case (FSharpValue.GetUnionFields(d.Activation,typeof<Activation>) |> fst) 
                PBias       , Case (FSharpValue.GetUnionFields(d.Bias,typeof<Bias>) |> fst)
            ]
        | ModuleSpecies s ->
            [
                PSpecies    , Class {TotalClasses=cfg.NumSpecies; Refs=[s]}
            ]
        | Norm l ->
            [
                PNorm       , Case (FSharpValue.GetUnionFields(l,typeof<NormalizationType>) |> fst)
            ]
        | _      -> printfn "unexpected node type"; []

    let cases xs   = xs |> List.map (function Case u  -> u | _ -> failwith "not expected")
    let conts xs   = xs |> List.map (function Cont f  -> f | _ -> failwith "not expected")

    let classes xs = 
        xs 
        |> List.collect (function Class c -> c.Refs |> List.map (fun r->c.TotalClasses,r) | _ -> failwith "not expected")
        |> List.groupBy fst
        |> List.map(fun (k,xs) -> {TotalClasses=k; Refs=xs |> List.map snd})
        |> List.head


    ///keep non-zero prob for all available options
    let completeCases (xs:(UnionCaseInfo*float) list)= 
        let uc,_ = xs.[0]
        let usc = xs |> List.map fst |> List.map(fun u->u.Name) |> set
        let allucs = FSharp.Reflection.FSharpType.GetUnionCases(uc.DeclaringType)
        let missingUcs = allucs |> Array.filter (fun x->usc.Contains x.Name |> not) |> Array.toList
        let mws = missingUcs |> List.map (fun x -> x, 0.01)
        List.append xs mws

    ///keep non-zero prob for all available options
    let completeClasses ci = 
        let allClasses = [for i in 0..ci.TotalClasses-1 -> i] 
        let weights = ci.Refs |> List.countBy yourself |> List.map (fun (x,c)->x,float c)
        let refCls = ci.Refs |> set
        let missingCs = allClasses |> List.filter (fun c -> refCls.Contains c |> not)
        let msw = missingCs |> List.map (fun x -> x, 0.01)
        List.append weights msw 

    ///aggregate list of parameter values. 
    ///each element of the list is expected to be of the same type
    //the form of 'aggregation' is dependent on the parameter type
    let aggregateParms xs =
        match xs with
        | []            ->  None
        | Case _::_     ->  xs 
                            |> cases 
                            |> List.countBy yourself 
                            |> List.map (fun (x,c)->x,float c) 
                            |> completeCases
                            |> List.toArray
                            |> createWheel          //create prob. 'wheel' for cases
                            |> snd
                            |> Cases |> Some
        | Cont _::_     ->  xs 
                            |> conts 
                            |> List.toArray 
                            |> Density |> Some      //create list for kernel density sampling
        | Class _::_    ->  xs 
                            |> classes
                            |> completeClasses
                            |> List.toArray
                            |> createWheel          //create prob. 'wheel' for classes 
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


