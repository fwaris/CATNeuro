#load "References.fsx"
open GELang.G

open Microsoft.Msagl.GraphViewerGdi

module GraphDrawing =
    type NodeM = Microsoft.Msagl.Drawing.Node
    open Microsoft.Msagl.Drawing

    let makeGraph (g:GELang.G.Graph) =
        let nodes = g.Nodes |> Map.toList |> List.map (fun (Id s,_) -> NodeM(s))    
        let gr = new Microsoft.Msagl.Drawing.Graph()
        nodes |> List.iter gr.AddNode
        g.Conns |> List.iter (fun c-> 
            let (Id f) = c.From
            let (Id t) = c.To
            let on = if c.On then "On" else ""
            gr.AddEdge(f,on,t)|> ignore)
        gr

let showGraph title g  = 
  let gv = new Microsoft.Msagl.GraphViewerGdi.GViewer()
  let gr = GraphDrawing.makeGraph g
  gv.Graph <- gr
  let f = new System.Windows.Forms.Form()
  f.Text <- title
  f.SuspendLayout()
  gv.Dock <- System.Windows.Forms.DockStyle.Fill
  f.Controls.Add(gv)
  gv.Invalidate()
  gv.Update()
  f.ResumeLayout()
  f.Show()



