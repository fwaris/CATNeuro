﻿namespace CATNeuro
open System.Threading.Tasks
open CAEvolve
open CATProb

module rec TopographicKS = 
    let policy =
        [|
            Crossover           , 0.5
            AddNode             , 0.1
            AddConnection       , 0.1
            MutateParm          , 0.00
            ToggleConnection    , 0.00
        |]
        |> createWheel

    let acceptance ca cfg species (st,topG) =
        let tst = st.TpState
        let tst' = updateState ca tst topG
        let st' = {st with TpState=tst'}
        (st',topG)

    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = 
        let tpst = st.TpState
        let indvs' =
            indvs
            |> Array.map (fun indv -> 
                let cntrd = CATProb.spinWheel tpst.SpinWheel 
                let p2 = cntrd.Best
                let influencedIndv = 
                    if RNG.Value.NextDouble() < 0.3 then 
                        let c2 = tpst.SpinWheel |> Array.filter(fun (c,_)->c<>cntrd) |> createWheel |>CATProb.spinWheel
                        {indv with Graph = c2.Best.Graph}
                    else
                        indv
                    
                evolveIndv cfg st speciesType policy (Some p2) influencedIndv)
        st,indvs'


    let MAX_INDVS = 30

    let updateState ca tst (topG:Individual[]) =
        let indvs =
            Seq.append tst.CIndvs topG
            |> Seq.toArray
            |> CAUtils.rankIndvs ca
            |> Array.truncate MAX_INDVS

        let k = match indvs.Length with 
                | x when x < 10 -> 2 
                | x when x < 20 -> 4 
                | x when x < 100 -> 5 
                | x when x < 500 -> 7 
                | _ -> 10

        let kcntrods,_ = KMeansClustering.kmeans cdist cfact cavg indvs k

        let cntrds = 
            kcntrods 
            |> Seq.filter (fun (_,ls)->List.isEmpty ls |> not) 
            |> Seq.map (toCentroid ca) 
            |> Seq.toList

        let wheel = cntrds |> Seq.map (fun c->c,float c.Count) |> Seq.toArray |> CATProb.createWheel

        {Centroids=cntrds; SpinWheel=wheel; CIndvs=indvs}


    let toCentroid ca (c,members) : Centroid =
        let lbest = CAUtils.rankIndvs ca members |> Seq.head
        {
            Center = c
            Count  = Seq.length members
            Best = lbest
        }

    let cfact xs k =  KMeansClustering.randomCentroids CATProb.RNG.Value xs k |> List.map (fun (x:Individual)->x,[])
    let cdist (x,_) y = GraphOps.distGraph x.Graph y.Graph
    let cavg (c,_) xs = c::xs |> center 

    let center xsList =
        let xs = xsList |> List.toArray
        let totalDists = Array.zeroCreate xs.Length
        Parallel.For(0,xs.Length,(fun i -> 
            let dists = Array.zeroCreate xs.Length

            Parallel.For(0,xs.Length, (fun j -> 
                    if i=j then 
                        dists.[j] <- 0.0
                    else
                        dists.[j] <- GraphOps.distGraph xs.[i].Graph xs.[j].Graph
                )) |> ignore

            totalDists.[i] <- Array.sum dists
        )) |> ignore
        
        let i,d = totalDists |> Array.mapi (fun i d-> i,d) |> Array.minBy snd
        xs.[i],xsList //'center' individual of the group and the group

        

(*
Topopgraphic keeps clusters of best performers
and does crossover between clusters
*)


