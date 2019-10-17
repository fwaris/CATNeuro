namespace rec CATNeuro

type Node = {Id:Id; Type:NodeType}
type Id = Id of string
type NodeType = Cell of CellType | Output of Dense | Input of string | ModInput | ModOutput
type Conn = {On:bool; From:Id; To:Id; Innovation:int}
type NormalizationType = BatchNorm | LayerNorm
type CellType = 
    | ModuleSpecies of int 
    | Dense of Dense
    | Norm of NormalizationType
    | SubGraph of Graph
type Bias = On | Off
type Dense = {Dims:int; Bias:Bias; Activation:Activation}
type Activation = NONE | Elu | Relu | LeakyRelu | Sig
type LearningParms = {Rate:float;} with static member Default = {Rate=0.01}
type Graph = {Nodes:Map<Id,Node>; Conns:Conn list}

type IdGen() =
    let mutable nodeId = 0
    let mutable connId = 0
    member x.node() = let i = System.Threading.Interlocked.Increment(&nodeId) in string i
    member x.conn() = let i = System.Threading.Interlocked.Increment(&connId) in i

type Range = {Lo:float; Hi:float}

type SpeciesType = Blueprint | Module of int

type IndvidualType = BlueprintIndv of LearningParms | ModuleIndv

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

type Knowledge  = Situational | Historical | Normative | Topgraphical | Domain 
type Individual = {Fitness:float[]; Graph:Graph; Id:int; IndvType:IndvidualType; KS : Knowledge}
type Population = {Species:SpeciesType; Individuals:Individual[]; Cfg:Cfg}

type SpeciesIndv = {SpeciesId:int; IndvidualId:int}

type AssembledModel = {Parms:LearningParms; Graph:Graph} 
type AssemblyMeta = {Gen:int; BestFit:float option}

type NetworkAssembly = {BlueprintId:int; Meta:AssemblyMeta;  Model:AssembledModel; ModuleReplacements:SpeciesIndv[] }

type Settings = {TakeFraction: float} with static member Default = {TakeFraction=0.25}

type Network = Individual[] ->  int -> Individual[]

type CA =
    {
        Populations  : Population[]
        Evaluator    : NetworkAssembly -> int*float[] //multi objective
        ParetoRank   : (int*float[])[] -> int[]
        Settings     : Settings
        Network      : Network
    }

type ConnMatch = 
    | Same      of Conn 
    | Diff      of Left:Conn*Right:Conn 
    | FrmL      of Conn 
    | FrmR      of Conn
    | ExtraR    of Conn
    | ExtraL    of Conn

