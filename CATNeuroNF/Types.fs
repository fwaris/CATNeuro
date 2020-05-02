(*
All main data types used for CATNeuro
*)
namespace rec CATNeuro

open Microsoft.FSharp.Reflection

type Node = {Id:Id; Type:NodeType}
type Id = Id of string
type NodeType = Cell of CellType | Output of Dense | Input of string | ModInput | ModOutput
type Conn = {On:bool; From:Id; To:Id; Innovation:int}
type Bias = On | Off
type Dense = {Dims:int; Bias:Bias; Activation:Activation}
type Conv2D = {Kernel:int; Filters:int; Stride:int; Activation:Activation}
type Activation = NONE | Elu | Relu | LeakyRelu | Sig |  TanH
type LearningParms = {Rate:float;} with static member Default = {Rate=0.01}
type Graph = {Nodes:Map<Id,Node>; Conns:Conn list}

type NormalizationType = BatchNorm | LayerNorm

type CellType = 
    | ModuleSpecies of int 
    | Dense of Dense
    | Conv2D of Conv2D
    | Norm of NormalizationType
    | SubGraph of Graph


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
        MaxNodes            : int
        NumSpecies          : int
        EliteFraction       : float
        WtSlctn_NormNode    : float 
        WtSlctn_DenseNode   : float
        WtSlctn_CovnNode    : float //set to 0. for no convolution 
        AllowDropInputs     : bool
        DenseRange          : Range
        KernelRange         : Range
        FiltersRange        : Range
        StrideRange         : Range
        LearnRange          : Range
        IdGen               : IdGen
    }
    static member Default() = {  
                                 MaxNodes           = 10
                                 NumSpecies         = 2; 
                                 EliteFraction      = 0.5
                                 WtSlctn_NormNode   = 0.1
                                 WtSlctn_DenseNode  = 1.0
                                 WtSlctn_CovnNode   = 0.0
                                 AllowDropInputs    = false
                                 IdGen              = IdGen()
                                 DenseRange         = {Lo=5.;    Hi=20.}
                                 KernelRange        = {Lo=2.;    Hi=20.}
                                 FiltersRange       = {Lo=2.;    Hi=200.}
                                 StrideRange        = {Lo=1.;    Hi=10.}
                                 LearnRange         = {Lo=0.001; Hi=0.1}
                              }

type Knowledge  = Situational | Historical | Normative | Topgraphical | Domain 

type Mutation = MutateParm | ToggleConnection | AddConnection | AddNode | Crossover

type Individual = 
    {  
        Id           : int
        Fitness      : float[]
        Graph        : Graph
        IndvType     : IndvidualType
        KS           : Knowledge 
        Restrictions : Set<Mutation>
    }

type Population = {Species:SpeciesType; Individuals:Individual[]; Cfg:Cfg}

type KnoweldgeDist = Stag_Hunt | Wtd_Majority

type SpeciesIndv = {SpeciesId:int; IndvidualId:int}

type AssemblyMeta = {Gen:int; BestFit:float option; Parms:LearningParms;}

type NetworkAssembly = {BlueprintId:int; Meta:AssemblyMeta;  Model:Graph; ModuleReplacements:SpeciesIndv[] }

type Settings = {TakeFraction: float} with static member Default = {TakeFraction=0.25}

type Network = Individual[] ->  int -> Individual[]

type CA =
    {
        Populations   : Population[]
        Evaluator     : NetworkAssembly -> int*float[] //multi objective
        ParetoRank    : (int*float[])[] -> int[]
        Settings      : Settings
        Network       : Network
        KnoweldgeDist : KnoweldgeDist
    }

type ConnMatch = 
    | Same      of Conn 
    | Diff      of Left:Conn*Right:Conn 
    | FrmL      of Conn 
    | FrmR      of Conn
    | ExtraR    of Conn
    | ExtraL    of Conn

type ShState = 
    {
        FitnessAtInit   : Map<int,float[]>
        GensSinceInit   : int
        CoopGens        : int
    }

type HsState = {Events:Individual list; Window:int}

type DmState = {Gens:int}
type SiState = {Exemplars:Individual[]; SpinWheel:(Graph*float)[]}

type CaseWheel = {Samples:int; CWheel:(UnionCaseInfo*float)[]}
type IntWheel = {Samples:int; IWheel:(int*float)[]}
type ParmType = PDims | PActivation | PBias | PSpecies | PNorm | PLearnRate
type ClassInfo = {TotalClasses:int; Refs:int list}
type DistVal = Case of UnionCaseInfo | Cont of float | Class of ClassInfo
type Dist = Cases of CaseWheel | Density of float[] | Classes of IntWheel

type NmState =
    {
        TopIndv : Individual[]
        MaxIndv : int
        Norms   : Map<int,Map<ParmType,Dist>> //norms organized by innovation#
    }

type Centroid =
    {
        Center  : Individual
        Count   : int
        Best    : Individual
    }

type TpState = 
    {
        Centroids : Centroid list
        CIndvs    : Individual[]
        SpinWheel : (Centroid*float)[]
    }

type CAState = 
    {
        Gen     : int
        ShState : ShState
        HsState : HsState
        DmState : DmState
        NmState : NmState
        TpState : TpState
        SiState : SiState
    }

type EvaluatedAssembly = {Assembly:NetworkAssembly; Fit:float[]}
type TimeStep= {CA:CA ; Best:EvaluatedAssembly[]; Count:int; State:Map<SpeciesType,CAState>}
