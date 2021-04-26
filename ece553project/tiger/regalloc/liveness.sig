structure FG = FuncGraph(struct type ord_key = Temp.temp val compare = Int.compare end)
signature Liveness=
sig
   
	      datatype igraph =
		       IGRAPH of {graph: Temp.temp FG.graph,
				  tnode: Temp.temp -> Temp.temp FG.node,
				  gtemp: Temp.temp FG.node -> Temp.temp,
				  moves: (Temp.temp FG.node * Temp.temp FG.node) list}
	      val interferenceGraph:
		  Flow.flowgraph ->
		  igraph *(FlowGraphGenerator.block InstGraph.node -> Temp.temp list)
	      val show: TextIO.outstream * Temp.temp FG.graph -> unit
	  end
	      
	      
