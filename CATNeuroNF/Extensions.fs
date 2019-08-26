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