#load "SetEnv.fsx"
open GELang
open G
open GOps
open GEDiag

let sampleGraph() =
    let last = {Id=Id"out"; Type = Output {Dims = 2; Bias=false; Activation=NONE}}

    let inter1 = 
                 {
                    Id = Id"inter1"
                    Type =               
                            Cell  {
                                        PreBN = false
                                        Op = Dense {Dims =6; Bias=false; Activation=NONE}
                                        PostBN = false
                                  }
                 }

    let inter2 = 
                {
                   Id = Id"inter2"
                   Type =               
                           Cell  {
                                       PreBN = false
                                       Op = Dense {Dims = 5; Bias=false; Activation=NONE}
                                       PostBN = false
                                 }
                }
    let inter3 = 
                {
                   Id = Id"inter3"
                   Type =               
                           Cell  {
                                       PreBN = false
                                       Op = Dense {Dims = 10; Bias=false; Activation=NONE}
                                       PostBN = false
                                 }
                }
    let inter4 = 
                {
                   Id = Id"inter4"
                   Type =               
                           Cell  {
                                       PreBN = false
                                       Op = Dense {Dims = 3; Bias=false; Activation=NONE}
                                       PostBN = false
                                 }
                }

    let input1 =  {Id=Id"input1"; Type = Input }

    let conns = 
        [
            {On=true; Innovation=0; From=inter1.Id; To=last.Id}
            {On=true; Innovation=1; From=inter2.Id; To=last.Id}
            //{On=true; Innovation=2; From=inter2.Id; To=inter1.Id}
            {On=true; Innovation=3; From=inter3.Id; To=inter2.Id}
            {On=true; Innovation=4; From=inter1.Id; To=inter3.Id}
            {On=true; Innovation=5; From=inter1.Id; To=inter2.Id}
            {On=true; Innovation=6; From=input1.Id; To=inter1.Id}
            {On=true; Innovation=7; From=inter1.Id; To=inter4.Id}
        ]
        
    {Nodes=[last; inter1; inter2; inter3; inter4; input1] |> List.map(fun i->i.Id,i) |> Map.ofList; Conns=conns}

let g = sampleGraph()

(*
SetEnv.showGraph "sample 1" g
*)
let gt = g |> trimGraph
SetEnv.showGraph "sample 1 trim" gt

let cfg = {MinCellDims=10; MaxCellDims=20; MaxNodes=10}

let dmpC g = g.Conns |> List.iter (printConn >> printfn "%s" )

let testToggle() =
    let g1 = GOps.toggleConnection cfg g
    g1 = g1 

    let g2 = GOps.toggleConnection cfg g1

    g2 = g1
    g2 = g


    g1.Conns |> List.map printConn
    g.Conns |> List.map printConn
    g2.Conns|> List.map printConn

    diffConn g1 g2
    diffConn g g2

let testAddConn() =
    let g1 = addConnection cfg g
    let g2 = addConnection cfg g1
    let g3 = addConnection cfg g2
    SetEnv.showGraph "g1" g1
    SetEnv.showGraph "g" g
    SetEnv.showGraph "g2" g2
    SetEnv.showGraph "g3" g3
    dmpC g 
    dmpC g1
    dmpC g2
    diffConn g1 g

