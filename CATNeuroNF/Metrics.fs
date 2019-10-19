module Metrics

open System.Threading
open System

//logging and metrics
//any component in the system can post messages
//defined here to log or gather metrics

type Parm =
    | MDensity of string * float[]
    | MCat of string * (string*float[])[]

type MetricMsg = 
  | NewGen of int
  | NodeAdd of int
    
let metricsToken = new CancellationTokenSource()
let obsAll,postAll = Observable.createObservableAgent<MetricMsg> metricsToken.Token

