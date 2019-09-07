namespace CATNeuro
open System.Threading.Tasks

module rec TopographicKS = 

    let acceptance ca cfg species (st,topG) =
        let tst = st.TpState
        let tst' = updateState ca tst topG
        let st' = {st with TpState=tst'}
        (st',topG)

    let updateState ca tst topG =
        let indvs =
            Seq.append tst.CIndvs topG
            |> Seq.toArray
            |> CAUtils.rankIndvs ca
            |> Array.truncate MAX_INDVS

        let k = match vns.Length with 
                | x when x < 10 -> 2 
                | x when x < 20 -> 4 
                | x when x < 100 -> 5 
                | x when x < 500 -> 7 
                | _ -> 10

        let kcntrods,_ = KMeansClustering.kmeans cdist cfact cavg  parmsArray k


    let MAX_INDVS = 1000
    let cfact xs k =  KMeansClustering.randomCentroids Probability.RNG.Value xs k |> List.map (fun (x:Individual)->x,[])
    let cdist (x,_) y = GraphOps.distGraph x.Graph y.Graph
    let cavg (c,_) xs = c::xs |> List.toArray |> center 

    let center xs =
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
        xs.[i]

        
    let influence ca cfg speciesType st  (topP:Individual[]) (indvs:Individual[]) = st,indvs

(*
Topopgraphic keeps clusters of best performers
and does crossover between clusters
*)


