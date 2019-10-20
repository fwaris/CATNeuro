module Metrics

open System.Threading
open System

//logging and metrics
//any component in the system can post messages
//defined here to log or gather metrics

type Parm =
    | MDensity of int * string * float[]
    | MCat of int * string * (string*float[])[]

type PopId = B | M of int

type MetricMsg = 
  | NewGen  of int
  | NodeAdd of (PopId*int)
  | Norms   of (PopId*Parm[])
    
let metricsToken = new CancellationTokenSource()
let obsAll,postAll = Observable.createObservableAgent<MetricMsg> metricsToken.Token

