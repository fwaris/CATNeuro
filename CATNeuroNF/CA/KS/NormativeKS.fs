namespace CATNeuro
open Ext
open FSharp.Reflection
open CATProb
open CAEvolve

module rec NormativeKS =     
    let policy =
        [|
            //Crossover           , 0.1
            //AddNode             , 0.1
            AddConnection       , 0.1
            MutateParm          , 0.7
            ToggleConnection    , 0.2
        |]
        |> createWheel

    let logNorms species nmst = 
        let mp = 
            nmst.Norms 
            |> Map.toArray 
            |> Array.collect(fun (i,m) -> m 
                                         |> Map.toArray 
                                         |> Array.map (fun (p,d)->  
                                            let pstr = sprintf "%A" p
                                            match d with
                                            | Density _  -> CnMetrics.MDensity (i,pstr,[||])
                                            | Cases _    -> CnMetrics.MCat (i,pstr,[||])
                                            | Classes _  -> CnMetrics.MCat (i,pstr,[||])
                                         ))
        (MUtils.popId species,mp) |> CnMetrics.Norms |> CnMetrics.postAll
        
    let acceptance ca cfg species (st,topG) =
        let nmst = st.NmState
        let nmst' = updateState ca cfg nmst topG
        logNorms species nmst'
        let st' = {st with NmState=nmst'}
        (st',topG)

    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = 
        let nmst = st.NmState
        let indvs' = indvs |> Array.map (evolveIndv cfg st speciesType policy None)
        st,indvs'

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

            |> Seq.append (highPerf |> Seq.choose (fun indv -> match indv.IndvType with 
                                                               | BlueprintIndv lr -> Some((EvolveParm.META_INV,PLearnRate),Cont (float lr.Rate)) 
                                                               | _ -> None))
            |> Seq.groupBy fst                                                                            //group by innov#
            |> Seq.map (fun ((i,p),xs) -> i,p, xs |> Seq.map snd |> Seq.toList |> aggregateParms)         //aggregate group to get parm distributions
            |> Seq.choose (fun (i,p,agg) -> agg |> Option.map(fun agg -> i,(p,agg)))                      //filter None
            |> Seq.groupBy fst                                                                            //re-group by innov# 
            |> Seq.map (fun (i,xs) -> i, xs |> Seq.map snd |> Map.ofSeq)                                  //innov# * (parmType->distribution) map
            |> Map.ofSeq                                                                                  //map with innov# as key
        {st with Norms=norms}

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


