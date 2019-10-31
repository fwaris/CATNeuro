namespace CATNeuro
module CnMetrics =

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
      | NewGen       of int
      | NodeAdd      of PopId
      | NodeAddMiss  of PopId
      | ConnAdd      of PopId
      | ConnAddMiss  of PopId
      | Xfer         of PopId
      | XferMiss     of PopId
      | CnnTggle     of PopId
      | CnnTggleMiss of PopId
      | ParmMutate   of PopId
      | Norms        of (PopId*Parm[])
    
    let metricsToken = new CancellationTokenSource()
    let obsAll,postAll = Observable.createObservableAgent<MetricMsg> metricsToken.Token

