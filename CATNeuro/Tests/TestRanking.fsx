#load "CNSetEnv.fsx"
open CATNeuro
open CATProb


let ranked = [| 1;2;3;4;5;6|]

let calcScaledRank id =
    let myRank = ranked |> Array.findIndex(fun i -> i = id) |> float
    let pX = 1.0 / (float ranked.Length)
    let myCumDnsty = myRank * pX
    let scaledRank = scaler (0.0,5.0) (0.0, 1.0) myCumDnsty |> int
    myRank,scaledRank

ranked 
    |> Array.map (fun i -> 
        let myRank,scaledRank = calcScaledRank i 
        {|Id=i; MyRank=myRank; SRank=scaledRank|}) 
    |> Array.iter (printfn "%A")
