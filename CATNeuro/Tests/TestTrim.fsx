#load "CNSetEnv.fsx"
open CATNeuro
open CATNeuro.GraphDiag
open CATNeuro.GraphOps
open CNSetEnv

let conns =
    [
        {   On = false
            From = Id "1"
            To = Id "2"
            Innovation = 1 }
        {   On = true
            From = Id "2"
            To = Id "3"
            Innovation = 2 }
        {   On = false
            From = Id "1"
            To = Id "9"
            Innovation = 18 }
        {   On = true
            From = Id "9"
            To = Id "2"
            Innovation = 19 }
        {   On = true
            From = Id "1"
            To = Id "17"
            Innovation = 41 }
        {   On = true
            From = Id "17"
            To = Id "9"
            Innovation = 42 }
        {   On = false
            From = Id "1"
            To = Id "19"
            Innovation = 45 }
        {   On = true
            From = Id "19"
            To = Id "2"
            Innovation = 46 }
    ]

let nodes =
    [
        {Id=Id"1"; Type= Input "a1"}
        {Id=Id"17"; Type= Cell (ModuleSpecies 0)}
        {Id=Id"19"; Type= Cell (ModuleSpecies 0)}
        {Id=Id"2"; Type= Cell (ModuleSpecies 0)}
        { Id = Id "3"
          Type = Output { Dims = 2
                          Bias = Off
                          Activation = NONE }}
        {Id=Id"9"; Type= Cell (ModuleSpecies 0)}
    ]

let g = {Nodes=nodes |> List.map(fun n->n.Id,n) |> Map.ofList; Conns=conns}

trimGraph g