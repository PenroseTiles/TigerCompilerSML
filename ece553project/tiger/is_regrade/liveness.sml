structure Liveness: Liveness =
struct
structure FuncGraph = FuncGraph(struct type ord_key = Temp.temp
				       val compare = Int.compare
				end)
(*type liveSet = unit Temp.Table.table*Temp.temp list*)
type liveSet = Temp.temp list
type liveMap = liveSet UseDefTable.table (*nodeID (from flowgraph)-> temp list*)
exception eexception
(*
val ids = FuncGraph.Table.listKeys(liveMap)
for id in ids:
    fun getTemps (id,ls) =ls @ valOf(FuncGraph.Table.look(liveMap, id) )
val allTemps = foldl getTemps [] ids
*)
 datatype igraph =
		       IGRAPH of {graph: Temp.temp FG.graph,
				  tnode: Temp.temp -> Temp.temp FG.node,
				  gtemp: Temp.temp FG.node -> Temp.temp,
				  moves: (Temp.temp FG.node * Temp.temp FG.node) list}
 fun printList([]) = print("End of list\n")
   | printList (a::l) = (print(Int.toString(a));print("\n");printList(l))

 fun lookupusedef(table, id):Temp.temp list =
     let val result' =  UseDefTable.look(table, id)
     in
	 case result' of SOME(x)=> x
		       | NONE =>[]
     end
	 
			    
 fun makeLiveMap(flow, []):liveMap = UseDefTable.empty
   | makeLiveMap(Flow.FGRAPH{control = graph, def = dest, use = source, ismove = im}, nodeList) = let
       fun makeEmptySet (nodelist) = Temp.Set.empty
       val blockids = map InstGraph.getNodeID nodeList
       (*val initSet = foldl makeEmptySet [] nodeList (*Create a list of empty sets, one for each node*)*)
       fun determine_livein (id, liveoutmap) =(* Find the livein set for a node *)
	   let val liveout = Temp.Set.fromList(lookupusedef(liveoutmap, id))
	       val useSet = case UseDefTable.look(source, id) of SOME(x) => Temp.Set.fromList(x)
							   | NONE => raise Fail("Did not find the use of node")
       in
	   (case UseDefTable.look(dest, id) of SOME(x) => Temp.Set.union(useSet, Temp.Set.difference(liveout, Temp.Set.fromList(x)))
					     | NONE => raise Fail("Did not find the def of node"))
       end
       fun updateLiveIn (id, (liveoutmap, liveinmap)) =
	   let val liveinofid = determine_livein(id, liveoutmap)
	   in  (liveoutmap, UseDefTable.enter(liveinmap, id, Temp.Set.toList(liveinofid)))
	   end
	       
       (* fun find_liveoutsets (nodeList) = *)
       (* 	   while (notsame == false) do *)
       (* 		 foldl [] nodeList *)
		 
       fun determine_liveout (blockid, liveoutmap, liveinmap):Temp.Set.set = (*Find the liveout set for a node*)
	   let val succList = InstGraph.succs(InstGraph.getNode(graph, blockid)) (*get succ using InstGraph.succs*)	       
	       fun findTemps ([]) = Temp.Set.empty
		 | findTemps (id::nodeList) = Temp.Set.union(determine_livein(id, liveoutmap), findTemps(nodeList))
	   in
	       findTemps(succList)
	   end
	       
       fun updateLiveOut (blockid:InstGraph.nodeID, (liveoutmap, liveinmap)) =
	   let val liveoutofid = determine_liveout(blockid, liveoutmap, liveinmap)
	   in (UseDefTable.enter(liveoutmap, blockid, Temp.Set.toList(liveoutofid)), liveinmap)
	   end
	       
       (*fun determine_liveout (succList: InstGraph.nodeID list):liveSet =
	  let (*get succ using InstGraph.succs*)
       fun findTemps ([]) = Temp.Set.empty
	 | findTemps (id::nodeList) = (case UseDefTable.look(source, id) of SOME(x) => Temp.Set.union(Temp.Set.fromList(x), findTemps(nodeList))
									  | NONE => raise Fail("Node was not found in the use table"))
	  in
	      Temp.Set.toList(findTemps(succList))
	  end*)
	      
      (* fun find_livesets ([]):liveSet list = [] *)
      (* 	| find_livesets ((id, block, succ, pred)::nodeList) = determine_liveout(succ)::find_livesets(nodeList) *)
      (* fun enter_liveout (id, liveset,livesetTable):liveMap = UseDefTable.enter(livesetTable, id, liveset) *)
      (* fun LiveSetFromNode (node, livesetTable) = *)
      (* 	  let *)
      (* 	      val blockid = InstGraph.getNodeID(node) *)
      (* 	      val livesetOfNode = determine_liveout(blockid)		   *)
      (* 	  in *)
      (* 	      enter_liveout(nodeid, livesetOfNode, livesetTable) *)
      (* 	  end *)
       fun updateBoth (blockid, (liveoutmap, liveinmap)) =
	   let val (_,liveinmap') = updateLiveIn(blockid, (liveoutmap, liveinmap))
	       val (liveoutmap', _) = updateLiveOut(blockid, (liveoutmap, liveinmap'))
	   in (liveoutmap', liveinmap')
	   end
	       
       fun checkLivenessEqual (blockid, (liveoutold, liveinold, liveoutnew, liveinnew)) =
	   let 
	   in Temp.Set.equal(Temp.Set.fromList(lookupusedef(liveoutold, blockid)),
			     Temp.Set.fromList(lookupusedef(liveoutnew, blockid))) andalso
	      Temp.Set.equal(Temp.Set.fromList(lookupusedef(liveinold, blockid)),
			     Temp.Set.fromList(lookupusedef(liveinnew, blockid)) )
	   end
	  
       fun checkMapsEqual (allmaps as (liveoutold, liveinold, liveoutnew, liveinnew)) =
	   let val notequals = List.filter (fn blockid => Bool.not(checkLivenessEqual(blockid, allmaps))) blockids
	   in
	       length(notequals)=0
	   end
	       
									     
       fun updateTillConvergence (blockids,liveoutmap, liveinmap) = let
	   val (liveoutmap', liveinmap') = foldl updateBoth (liveoutmap, liveinmap) blockids
       in if checkMapsEqual (liveoutmap, liveinmap, liveoutmap', liveinmap')
	  then liveoutmap
	  else updateTillConvergence(blockids, liveoutmap', liveinmap')
       end
									
	(*val initMap = foldl LiveSetFromNode UseDefTable.empty nodeList*)
   in
       
      updateTillConvergence(blockids, UseDefTable.empty, UseDefTable.empty)
      (* map enter_liveout (find_livesets((id, block, succ, pred)::nodeList)) *)
  end

(*Assuming liveIn and liveOut are two sets*)
fun interfereCalc(Flow.FGRAPH{control = _, def=defs, use=uses, ismove= ismovetable}, livenessMap(*blockID -> temp list*)) =
    let
	val startGraph = FG.empty (*start with an empty graph*)
	val blockids = UseDefTable.listKeys(livenessMap)
	fun getTemps (id,ls) =let in case UseDefTable.look(livenessMap, id) of
					 NONE => ls
				      |  SOME(x) => (*(print("BLOCK ");print(Int.toString(id));printList(x@ls);*) x@ls
			      end
				  
	val allTemps: Temp.temp list = foldl getTemps [] blockids
	fun addNewTempToIgraph (temp, graph) = FG.addNode(graph, temp, temp)
	(*fun newNodes(startingNode, startingGraph) = (*adds new nodes to the interference graph starting wtih one node*)
	    let		
		fun newTemp(temp, graph) = FuncGraph.addNode(graph, temp, temp) (*adds an empty temp to the graph*)
	    in
		foldl newTemp startingGraph (allTemps) (*folds over all of uses and definitions to add all of the temps to the graph*)
	    end*)
	(*after newNodes we have a graph will of the nodes based on uses and definitions but no edges*)
	val interGraph = foldl addNewTempToIgraph startGraph allTemps
	(* val () = print("ALL TEMPS\n") *)
	(* val () = printList (allTemps) *)
	(* val () = print("BLOCK IDS\n") *)
	(* val () = printList (blockids) *)

	fun drawEdges (id, graph) = (* *Igraph ->Igraph*)
	    let val liveout = valOf(UseDefTable.look(livenessMap, id))
		(*val () = print("FOUND LIVEOUT\n")*)
		(*val () = printList(liveout)*)
		val def = valOf(UseDefTable.look(defs, id))
		(*val () = print("FOUND DEF\n")*)
		(*val () = printList (def)*)
		val use = valOf(UseDefTable.look(uses, id))
		(*val () = print("FOUND USE\n")*)
		(*val () = printList(use)*)
		val ismove = valOf(UseDefTable.look(ismovetable, id))
		(*val () = if ismove then print("true") else print("false")*)
		fun outerfold (livetemp,g) =
		    let										   
			fun addEdge (deftemp, graph') = (*for eachlivetemp, add an edge bw deftemp and livetemp*)
			    (*check if definition is singular or if the temp is defined many times*)
			    (*if ismove is true, we need to check the uses of the node to see if the use and the temp are the same. if they are, we do not draw an edge, if they are not we draw an edge*)
			    if ismove
			    then
				if (livetemp=List.nth(use, 0)) (*source register of move is second use temp --> Anurag please check this*)
				then graph' (*just return graph, do not draw edge if c = bi*)
				else FG.doubleEdge(graph', deftemp, livetemp)
			    else FG.doubleEdge(graph', deftemp, livetemp)
			
		    in
			foldl addEdge g def (*draw edges on graph for all of the nodes that are liveout of the basic block*)
		    end
	    in
		foldl outerfold graph liveout
	    end
		
    in
	foldl drawEdges interGraph blockids (*draw edges based on liveness information on interference graph*)
    end

fun movesListCalc(Flow.FGRAPH{control=c, def=defs, use=uses, ismove=isMoveTable}, interGraph) =
    let
	fun helper (node, list):(Temp.temp FG.node*Temp.temp FG.node) list = (*TODO*)
	    let
		val defs:Temp.temp list = valOf(UseDefTable.look(defs, InstGraph.getNodeID(node)))
		val isMove:bool = valOf(UseDefTable.look(isMoveTable, InstGraph.getNodeID(node)))
		val uses:Temp.temp list = valOf(UseDefTable.look(uses, InstGraph.getNodeID(node)))
	    in		
		if (isMove andalso length(defs)=1 andalso length(uses)=1)
		(*get node info, if the instruction is a move, add to the list*) 
		then list @ [(FG.getNode(interGraph, List.nth(defs,0)), FG.getNode(interGraph, List.nth(uses,0)))]
		else list
	    end		
    in
	foldl helper [] (InstGraph.nodes(c)) (*check all nodes in flowgraph to see if they are a move*)
    end
	
fun show(outstream:TextIO.outstream, igraph:Temp.temp FG.graph):unit = (*TODO*)
    let
	val nodeList = FG.nodes(igraph)
	fun printNode (nodeid) = TextIO.output(outstream, Int.toString(nodeid)^"  ")
	fun printAdj (node) = let in app printNode (FG.adj(node));() end
	fun printEverythingForNode (node) = let in printNode(FG.getNodeID(node));
						  TextIO.output(outstream, " -> ");
						  printAdj(node);
						  TextIO.output(outstream, "\n")
					   end
					       
					       
    in
	app printEverythingForNode (FG.nodes(igraph));
	()
    end

fun printLiveMap(livemap) =
    let val blockids = UseDefTable.listKeys(livemap)
	fun printmapping blockid = let
	in print(Int.toString(blockid)^" -> \n");
	   printList(lookupusedef(livemap, blockid))
	end
    in
	app printmapping blockids			       
    end
	
	
fun interferenceGraph(flowgraph as Flow.FGRAPH{control=instgraph, def=d, use=u, ismove=im}) =
    let val fgnodes = InstGraph.nodes(instgraph)
	val liveOut = makeLiveMap(flowgraph, fgnodes) (*not sure where these arguments come frome*)
	val _ = printLiveMap(liveOut)
	(*val () = print("FIGURED OUT LIVE NODES\n")*)
	val interGraph = interfereCalc(flowgraph, liveOut)
	(*val () = print("FIGURED OUT INTERFERENCE GRAPH\n")*)
	val igraph = IGRAPH{
		graph = interGraph,
		tnode = (fn tempArg => FG.getNode(interGraph,tempArg)),
		gtemp = FG.getNodeID,
		moves = movesListCalc(flowgraph, interGraph)
	    }
	fun blockNodeToLiveSet (node: 'a InstGraph.node):Temp.temp list =
	    let val nodeID = InstGraph.getNodeID(node)
		val live' = UseDefTable.look(liveOut, nodeID)
		val ret = case live' of SOME(x)=> x
				      | NONE => raise eexception 
	    in
		ret
	    end
		
    in
	(igraph, blockNodeToLiveSet)
    end
	
end
