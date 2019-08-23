namespace rec CATNeuro


type Node = {Id:Id; Type:NodeType}
type Id = Id of string
type NodeType = Cell of Cell | Output of Dense | Input
type Conn = {On:bool; From:Id; To:Id; Innovation:int}
type Cell = 
    | Species of int 
    | Dense of Dense
    | BatchNorm
    | LayerNorm
type Dense = {Dims:int; Bias:bool; Activation:Activation}
type Activation = NONE | Elu | Relu | LeakyRelu | Sig
type LearningParms = {Rate:float;} with static member Default = {Rate=0.01}
type Graph = {Parms:LearningParms; Nodes:Map<Id,Node>; Conns:Conn list}

type IdGen() =
    let mutable nodeId = 0
    let mutable connId = 0
    member x.node() = let i = System.Threading.Interlocked.Increment(&nodeId) in string i
    member x.conn() = let i = System.Threading.Interlocked.Increment(&connId) in i

type Range = {Lo:float; Hi:float}

type SpeciesType = Blueprint | Module of int

type Cfg =
    {
        MaxNodes    : int
        NumSpecies  : int
        DenseRange  : Range
        LearnRange  : Range
        IdGen       : IdGen
    }
    static member Default =  {  NumSpecies=2; 
                                IdGen=IdGen()
                                DenseRange={Lo=5.; Hi=20.}
                                LearnRange={Lo=0.01; Hi=0.1}
                                MaxNodes=10;}

type Individual = {Fitness:float; Graph:Graph; Id:int}
type Population = {Species:SpeciesType; Individuals:Individual[]; Cfg:Cfg}

type CA =
    {
        Populations : Population[]
                   
    }

type Match = SameL | DiffL | FrmL | FrmR| ExsL | ExsR

        
            










            
        
        

