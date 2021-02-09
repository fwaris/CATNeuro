namespace CATNeuro

module Ext =
    let separate<'a> (xss : 'a[][]) = //row-wise
        if xss |> Array.isEmpty then 
            [||]
        else
            let h = xss.[0]
            h |> Array.mapi (fun i _ -> Array.init xss.Length (fun j -> xss.[j].[i])) //column-wise


    let combine<'a> (xss:'a[][]) = //column-wise
        if xss |> Array.isEmpty then
            [||]
        else    
            let h = xss.[0]
            h |> Array.mapi (fun i _ -> Array.init xss.Length (fun j -> xss.[j].[i])) //row-wise

    (*
    let xs = [|[|1;2|]; [|3;4|]; [|5; 6|] |]
    let xs' = separate xs
    let ys = combine xs'
    xs = ys
    *)

    let inline yourself x = x

    let isValidNum n = (System.Double.IsInfinity n || System.Double.IsNaN n) |> not

    let scaler (sMin,sMax) (vMin,vMax) (v:float) =
        //if v < vMin then failwith "out of min range for scaling"
        //if v > vMax then failwith "out of max range for scaling"
        let v = max v vMin
        let v = min v vMax
        (v - vMin) / (vMax - vMin) * (sMax - sMin) + sMin

    let makeBins (mn:float) mx bins = 
        if mn > mx then failwith "mn > mx"
        let r = mx - mn
        let bw = r / bins
        (mn,mn+bw,1) |> Seq.unfold (fun (mn,mx,c) -> 
            if c > int bins then
                None
            else
                Some ((mn,mx), (mx,mx+bw,c+1)))
        |> Seq.toList


    ///round robbin collect elements in the bins
    ///collect 1st element from 1st bin, 1st element from 2nd bin until no more bins
    ///then repeat starting from the 1st bin and 2nd element...
    let rec clct acc l1 l2 =
        match l1, l2 with 
        | [],[]             -> acc |> List.rev
        | [],_              -> clct acc (List.rev l2) []
        | (_,[])::rest,_    -> clct acc rest l2
        | (b,x::r1)::rest,_ -> clct (x::acc) rest ((b,r1)::l2)

    let pickle<'a> file (o:'a) =
        let ser = MBrace.FsPickler.FsPickler.CreateXmlSerializer()
        use strw = System.IO.File.CreateText(file)
        ser.Serialize(strw,o)

    let unpickle<'a> file =
        let ser = MBrace.FsPickler.FsPickler.CreateXmlSerializer()
        use strw = System.IO.File.OpenText file
        let o = ser.Deserialize<'a>(strw)
        o
        
