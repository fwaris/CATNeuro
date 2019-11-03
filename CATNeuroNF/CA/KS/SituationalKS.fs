namespace CATNeuro
open Ext
open CAEvolve
open CATProb
                   
module rec SituationalKS = 
    let policy =
        [|
            Crossover           , 0.0
            AddNode             , 0.1
            AddConnection       , 0.5
            MutateParm          , 0.1
            ToggleConnection    , 0.0
        |]
        |> createWheel

    let acceptance ca cfg species (st,topG:Individual[]) =
        let sist = st.SiState
        let sist' = updateState ca sist topG
        let st' = {st with SiState=sist'}
        (st',topG)

    let influence ca cfg speciesType st (topP:Individual[]) (indvs:Individual[]) = 
        let indvs' =
            indvs 
            |> Array.map (fun indv -> 
                let g = CATProb.spinWheel st.SiState.SpinWheel
                let indv' = {indv with Graph=g}
                let influencer = indv
                evolveIndv cfg st speciesType policy (Some influencer) indv')                
        st,indvs'

    let MAX_EXEMPLARS = 15

    let updateState ca sist topG =
        let explrs = 
            pickExemplars ca sist topG 
            |> List.truncate MAX_EXEMPLARS
            |> List.toArray

        let weights = 
            explrs
            |> Array.map (fun (indv:Individual) -> indv.Graph, indv.Fitness.[0])
        let wheel = CATProb.createWheel weights
        {sist with Exemplars=explrs; SpinWheel=wheel}

    ///select exemplars from existng and new elites.
    ///a binning mechanism is used to ensure greater 
    ///diversity of selected exemplars
    let pickExemplars ca st topG =
        let ranked = Seq.append st.Exemplars topG |> CAUtils.rankIndvs ca
        let best = ranked.[0]
        let exRest = ranked.[1..]

        //find 'distance' of the rest from best
        let divsM = 
            exRest
            |> Array.mapi (fun  i indv -> i, (GraphOps.distGraph best.Graph indv.Graph))
            |> Map.ofArray

        let (_,mxD) = divsM |> Map.toArray |> Seq.maxBy snd
        let (_,mnD) = divsM |> Map.toArray |> Seq.minBy snd
        let bins = makeBins mnD (mxD + 0.0001) 5. //put the distance spread into 5 bins

        //bin each example into the distance bin
        let binned = (Map.empty,exRest |> Array.mapi (fun i exm ->i,exm)) ||> Array.fold (fun acc (i,exm) -> 
            let d = divsM.[i]
            let b = bins |> List.find (fun (mn,mx) -> mn <= d && d < mx)
            match acc |> Map.tryFind b with
            | Some ls -> exm::ls
            | None    -> [exm]
            |> fun x -> acc |> Map.add b x)

        //sort bins
        let binned = //sort each bin by decreasing order of fitness
            binned 
            |> Map.map (fun k vs -> CAUtils.rankIndvs ca vs |> Array.toList)
            |> Map.toSeq
            |> Seq.sortBy (fun ((mn,mx),_) -> mx) //sort all bins by decreasing diversity
            |> Seq.toList
        best::(clct [] binned [])    

        
(*
Situational tracks examplars 
and performs add connection mutation 
to elite and population
*)

