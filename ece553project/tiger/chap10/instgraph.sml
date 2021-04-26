structure InstGraph = FuncGraph(struct type ord_key=int
				       val compare =Int.compare
				end)
structure UseDefTable = IntMapTable(struct type key=InstGraph.nodeID
					   val getInt = (fn x=> x) end
				   )
