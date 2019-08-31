namespace CATNeuro
open Ext
//internal state to run CA 
type ShState = 
    {
        FitnessAtInit   : Map<SpeciesType,Map<int,float[]>>
        GensSinceInit   : int
        CoopGens        : int
    }

type HsState = {Events:Individual list; Window:int}

type DmState = {EliteFrac:float}
 
type CAState = {ShState:ShState; Gen:int; HsState:HsState; DmState:DmState}

