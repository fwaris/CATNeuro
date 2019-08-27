namespace CATNeuro
open Ext

type FitHist = {Id:int; Fitness:float[]}
type ShState = 
    {
        FitnessAtInit   : Map<SpeciesType,FitHist[]>
        GensSinceInit   : int
        CoopGens        : int
        KSOrder         : Knowledge[]
        KSRange         : float*float
    }
 
type CAState = {ShState:ShState}

module rec BSpace =
    ()

