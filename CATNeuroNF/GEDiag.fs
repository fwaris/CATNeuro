﻿module GEDiag
open GELang
open G
open GOps

let printConn c = 
    let (Id f) = c.From
    let (Id t) = c.To
    let on = if c.On then "->" else "--"
    sprintf "%d.%s%s%s" c.Innovation f on t

type Match = Same | Diff | FrmL | FrmR| ExsL | ExsR

let diffConn (a:Graph) (b:Graph) =
    let acs = a.Conns |> List.sortBy (fun x -> x.Innovation)
    let bcs = b.Conns |> List.sortBy (fun x -> x.Innovation)
    
    let rec loop acc ls rs =
        match ls,rs with
        | [],[]                                              -> List.rev acc
        | [],r::rest                                         -> loop ((ExsR, r)::acc) ls rest
        | l::rest,[]                                         -> loop ((ExsL, l)::acc) rest rs
        | l::restL,r::restR when l = r                       -> loop ((Same,l)::acc) restL restR
        | l::restL,r::restR when l.Innovation = r.Innovation -> loop ((Diff,l)::acc) restL restR
        | l::restL,r::restR when l.Innovation < r.Innovation -> loop ((FrmL,l)::acc) restL rs
        | l::restL,r::restR                                  -> loop ((FrmR,l)::acc) ls restR

    loop [] acs bcs


