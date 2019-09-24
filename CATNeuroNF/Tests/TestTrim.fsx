#load "CNSetEnv.fsx"
open CATNeuro
open CATNeuro.GraphDiag
open CATNeuro.GraphOps
open CNSetEnv

let gmod : Graph =
  {Nodes =
      [(Id "1", {Id = Id "1";
                 Type = ModInput;});
       (Id "2", {Id = Id "2";
                 Type = Cell (Dense {Dims = 14;
                                     Bias = On;
                                     Activation = LeakyRelu;});});
       (Id "20", {Id = Id "20";
                  Type = Cell (Dense {Dims = 12;
                                      Bias = Off;
                                      Activation = Elu;});});
       (Id "3", {Id = Id "3";
                 Type = ModOutput;});
       (Id "30", {Id = Id "30";
                  Type = Cell (Dense {Dims = 16;
                                      Bias = On;
                                      Activation = Relu;});})]|> Map.ofList;
      
   Conns =
    [{On = false;
      From = Id "1";
      To = Id "2";
      Innovation = 1;}; {On = true;
                         From = Id "2";
                         To = Id "3";
                         Innovation = 2;}; {On = true;
                                            From = Id "1";
                                            To = Id "3";
                                            Innovation = 38;};
     {On = true;
      From = Id "20";
      To = Id "2";
      Innovation = 68;}; {On = true;
                          From = Id "1";
                          To = Id "20";
                          Innovation = 69;}; {On = true;
                                              From = Id "30";
                                              To = Id "2";
                                              Innovation = 102;};
     {On = false;
      From = Id "1";
      To = Id "30";
      Innovation = 103;}];}


let gmodt = GraphOps.trimGraph gmod
CNSetEnv.showGraph "trimmed" gmodt