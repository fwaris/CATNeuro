#load "CNSetEnv.fsx"
open CATNeuro
open CATNeuro.GraphDiag
open CATNeuro.GraphOps

let cfg = Cfg.Default
let p = LearningParms.Default

let sampleGraph() =
    let last = {Id=Id"out"; Type = Output {Dims = 2; Bias=Bias.Off; Activation=NONE}}

    let inter1 = 
                 {
                    Id = cfg.IdGen.node() |> Id
                    Type = Cell (Dense {Dims =6; Bias=Bias.Off; Activation=NONE})
                 }

    let inter2 = 
                {
                   Id = cfg.IdGen.node() |> Id
                   Type = Cell (Dense {Dims = 5; Bias=Bias.Off; Activation=NONE})              
                }
    let inter3 = 
                {
                   Id = cfg.IdGen.node() |> Id
                   Type = Cell (Dense {Dims = 7; Bias=Bias.Off; Activation=NONE})               
                }
    let inter4 = 
                {
                   Id = cfg.IdGen.node() |> Id
                   Type = Cell (Dense {Dims = 3; Bias=Bias.Off; Activation=NONE})
                }

    let input1 =  {Id=Id"input1"; Type = Input }


    let conns = 
        [
            {On=true; Innovation=cfg.IdGen.conn(); From=inter1.Id; To=last.Id}
            {On=true; Innovation=cfg.IdGen.conn(); From=inter2.Id; To=last.Id}
            //{On=true; Innovation=2; From=inter2.Id; To=inter1.Id}
            {On=true; Innovation=cfg.IdGen.conn(); From=inter3.Id; To=inter2.Id}
            {On=true; Innovation=cfg.IdGen.conn(); From=inter1.Id; To=inter3.Id}
            {On=true; Innovation=cfg.IdGen.conn(); From=inter1.Id; To=inter2.Id}
            {On=true; Innovation=cfg.IdGen.conn(); From=input1.Id; To=inter1.Id}
            {On=true; Innovation=cfg.IdGen.conn(); From=inter1.Id; To=inter4.Id}
        ]
        
    {Nodes=[last; inter1; inter2; inter3; inter4; input1] |> List.map(fun i->i.Id,i) |> Map.ofList; Conns=conns}

let g = sampleGraph()

(*
SetEnv.showGraph "sample 1" g
let gt = g |> trimGraph
SetEnv.showGraph "sample 1 trim" gt
*)


let dmpC g = g.Conns |> List.iter (printConn >> printfn "%s" )

let testToggle() =
    let gt1 = toggleConnection cfg g
    gt1 = gt1 
    SetEnv.showGraph "toggle connection"  gt1

    let gt2 = toggleConnection cfg gt1

    gt2 = gt1
    gt2 = g

    diffConn g gt1
    diffConn gt1 gt2

    dmpC g
    dmpC gt1
    dmpC gt2

let testAddConn() =
    let g1 = addConnection cfg g
    let g2 = addConnection cfg g1
    let g3 = addConnection cfg g2
    SetEnv.showGraph "add connection" g1
    SetEnv.showGraph "g" g
    SetEnv.showGraph "g2" g2
    SetEnv.showGraph "g3" g3
    dmpC g 
    dmpC g1
    dmpC g2
    diffConn g1 g

let testAddNode() =
    let g1 = addNode cfg g
    let g2 = addNode cfg g1
    SetEnv.showGraph "g" g
    SetEnv.showGraph "add node" g1
    SetEnv.showGraph "g2" g2
    let gn = (g,[1 .. 10]) ||> List.fold (fun x i -> printfn "i=%d" i; addNode cfg x)
    SetEnv.showGraph "gn" gn

let testCrossover1() =
    let g1 = addNode cfg g
    let g2 = addNode cfg g
    let g3 = crossover cfg g1 g2
    SetEnv.showGraph "g1" g1
    SetEnv.showGraph "g2" g2
    SetEnv.showGraph "crossover" g3

let testCrossover() = 
    let g1 = addNode cfg g
    let g2 = addNode cfg g1
    let g3 = crossover cfg g1 g2 |> addConnection cfg
    SetEnv.showGraph "g" g
    SetEnv.showGraph "g1" g1
    SetEnv.showGraph "g2" g2
    SetEnv.showGraph "g3" g3
