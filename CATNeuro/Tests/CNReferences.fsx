﻿
//#r @"..\..\packages\FsPickler.5.2.2\lib\net45\FsPickler.dll"
//#r @"..\..\packages\Microsoft.Msagl.1.1.3\lib\net40\Microsoft.Msagl.dll"
//#r @"..\..\packages\Microsoft.Msagl.Drawing.1.1.3\lib\net40\Microsoft.Msagl.Drawing.dll"
//#r @"..\..\packages\Microsoft.Msagl.GraphViewerGDI.1.1.3\lib\net40\Microsoft.Msagl.GraphViewerGdi.dll"
//#r @"..\..\packages\FSharp.Charting.2.1.0\lib\net45\FSharp.Charting.dll"
//#r @"..\..\packages\MathNet.Numerics.4.9.0\lib\net461\MathNet.Numerics.dll"
//#r @"..\..\packages\MathNet.Numerics.FSharp.4.9.0\lib\net45\MathNet.Numerics.FSharp.dll"

#r "nuget: FsPickler"
#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: Microsoft.Msagl.GraphViewerGDI"
#r "nuget: FSharp.Charting"            //need different version here

#r "System.Windows.Forms"
#r "System.Windows.Forms.DataVisualization"
#load @"..\Extensions.fs"
#load @"..\CATProb.fs"
#load @"..\Metrics\CnObservableExt.fs"
#load @"..\Metrics\CnMetrics.fs"
#load @"..\..\CATNeuroCharts\CnTraceCharts.fs"
#load @"..\KMeans.fs"
#load @"..\Types.fs"
#load @"..\Extensions.fs"
#load @"..\Graph\GraphOps.fs"
#load @"..\Graph\GraphDiag.fs"
#load @"..\CA\CAUtils.fs"
#load @"..\CA\CAEvolve.fs"
#load @"..\CA\KDStagHunt.fs"
#load @"..\CA\StagHuntStoch.fs"
#load @"..\CA\KDStagHuntAdapt.fs"
#load @"..\CA\KDWtdMajority.fs"
#load @"..\CA\KS\DomainKS.fs"
#load @"..\CA\KS\NormativeKS.fs"
#load @"..\CA\KS\HistoryKS.fs"
#load @"..\CA\KS\SituationalKS.fs"
#load @"..\CA\KS\TopographicKS.fs"
#load @"..\CA\BeliefSpace.fs"
#load @"..\CA\CARunner.fs"
#load @"..\NEAT\ElitistKS.fs"
#load @"..\NEAT\ElitistBeliefSpace.fs"
#load @"..\NEAT\ElitistRunner.fs"