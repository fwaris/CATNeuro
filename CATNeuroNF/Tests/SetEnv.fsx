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
            let l = sprintf "%d" c.Innovation 
            let e = gr.AddEdge(f,l,t)
            e.UserData <- c.On
            )
        gr

let styleEdge (e:Microsoft.Msagl.Drawing.Edge) =
    match e.UserData with
    | :? bool as b when not b -> e.Attr.AddStyle(Microsoft.Msagl.Drawing.Style.Dotted)
    | _ -> ()

let showGraph title g  = 
  let gv = new Microsoft.Msagl.GraphViewerGdi.GViewer()
  let gr = GraphDrawing.makeGraph g
  gr.Edges |> Seq.iter styleEdge
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



