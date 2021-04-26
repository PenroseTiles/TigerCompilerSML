(*    datatype flowgraph = FGRAPH of {control: Graph.graph,
def: Temp.temp list Graph.Table.table, use: Temp.temp list Graph.Table.table, ismove: bool Graph.Table.table}
type 'a node = (nodeID * 'a * NodeSet.set * NodeSet.set)
type 'a graph = 'a node NodeMap.map
type 'a edge = {from: nodeID, to: nodeID} *)
signature MakeGraph=
sig
    type block
    val instrs2graph: Assem.instr list -> Flow.flowgraph *  block InstGraph.node list
end

structure FlowGraphGenerator:>MakeGraph=
struct
type block = Assem.instr list
type idblock = InstGraph.nodeID*block

fun printList([]) = print("End of list\n")
  | printList (a::l) = (print(Int.toString(a));print(", ");printList(l))

structure A=Assem			       
exception ZipError
exception eexception	      
fun zip([],[])=[]
  | zip (x::xs, y::ys) = (x,y)::zip(xs, ys)
  | zip _ = raise ZipError

fun printInstGraph graph =
    let fun instr2str instr = Assem.format(MipsFrame.makestring) instr
	fun f (nodeID, block) = foldr (op ^) "" ("nodeID: "::Int.toString(nodeID)::"\n"::(map instr2str block))
    in InstGraph.printGraph(f)(graph)
    end
	
		  
fun instrs2graph ([])=(Flow.FGRAPH{control=InstGraph.empty,
			      def=UseDefTable.empty,
			      use=UseDefTable.empty,
			      ismove=UseDefTable.empty}, [])
  | instrs2graph (inslist) =
    let fun addNodeFoldable ((id, b), (graph,list)) =
	    let
		val (newGraph,newNode)= InstGraph.addNode'(graph, id, b)
	    in
		(newGraph, list@[newNode])
	    end
	fun createBlocks (completed, curblock, []) = completed @ [curblock]
	  | createBlocks (completed, [], (label as A.LABEL{...})::instrs)
	    = createBlocks(completed, [label], instrs)
	  | createBlocks (completed, curblock, (label as A.LABEL{...})::instrs)
	    = createBlocks(completed@[curblock], [label], instrs)
	  | createBlocks (completed, curblock, (instr as A.OPER{jump=SOME(labels), ...})::instrs)
	    = createBlocks(completed@[curblock@[instr]], [], instrs)
	  | createBlocks (completed, curblock, (instr as A.MOVE{...})::instrs)
	    = createBlocks(completed@[curblock@[instr]], [], instrs)
	  | createBlocks (completed, curblock, instr::instrs) =  createBlocks(completed, curblock@[instr], instrs)
							      
	val blocks:block list = map (fn x=> [x]) inslist (*createBlocks([],[], inslist)*)			  
	fun isLabel (label as A.LABEL{...})=true
	  | isLabel _ = false
	val numBlocks = List.length(blocks)				  
	val nodeIDs = List.tabulate(numBlocks, fn x => x)(* returns [0,1,...,numblocks-1] *)
	val IDblockPairs = zip(nodeIDs, blocks)
	fun snd (a,b)=b
	fun fst (a,b)=a
			  
	(*return true if f x is true for any x in list			  
	fun any f ([]) = false
	  | any f (x::xs) = (f x) orelse (any f xs)*)
	
	(*return true if block starts with a label instruction, I'm assuming labels can only occur at the start of a block*)
	fun startsWithLabel bloc = (isLabel o hd) bloc
	fun returnIfLabel (id:InstGraph.nodeID, bloc:block) = if startsWithLabel(bloc) 
				       then SOME(id, bloc)
				       else NONE							
	val LabelBlocksWithID = List.mapPartial returnIfLabel (IDblockPairs)
	(*build a set of all blocks containing labels*)

	(*if block starts with a label, return the label(symbol)*)
	fun LabelBlockToLabel ((labelinstr as A.LABEL{assem=_, lab=label})::instrs):Symbol.symbol = label
	  | LabelBlockToLabel _ = raise eexception
	(*fun LabelNodeToLabel node = LabelBlockToLabel(FuncGraph.nodeInfo(node))*)
	fun idLabelHelper (id:InstGraph.nodeID,bloc:block) = (id, LabelBlockToLabel(bloc))
	
	val IdLabelPairs= map idLabelHelper (LabelBlocksWithID)


	fun label2NodeIDhelper (lab, []) = raise eexception
	 |  label2NodeIDhelper (lab, (a,b)::pairs) = if (lab = b) then  a
						     else label2NodeIDhelper(lab, pairs)
							 							
	fun label2NodeID lab = label2NodeIDhelper(lab, IdLabelPairs)
	    
	(*add all blocks to the graph*)
	val (graphWithoutEdges, allNodes) = foldl addNodeFoldable (InstGraph.empty, []) IDblockPairs

	(*add all edges to graph*)
	fun addOutgoingEdges ((srcID,srcBlock), graph) =
	    let val lastInstr = List.last(srcBlock)
		fun getJump (A.OPER{jump=SOME(labels),...}) = labels
		  | getJump _ = []
		val jumpTo = getJump(lastInstr)
				    
		val destIDs = map label2NodeID jumpTo
		(*if block does not end with jump instrs then add an edge to next block*)
		val destIDs' = if (List.length(destIDs)=0) andalso (srcID<numBlocks-1) then (srcID+1)::destIDs else destIDs
		fun edgeTo (destID, ls) = {from=srcID, to=destID}::ls
		val edges = foldl edgeTo [] destIDs'
		fun addEdge' (a, b)= InstGraph.addEdge (b, a)
	    in
		foldl addEdge' graph edges
	    end
	val finalGraph = foldl addOutgoingEdges graphWithoutEdges IDblockPairs
	(*now compute def and use*)
	fun getDst (A.OPER{assem=_, dst=dst, src=_, jump=_}) = dst
	  | getDst (A.MOVE{assem=_, dst=dst, src=_}) = [dst]
	  | getDst (A.LABEL{...}) = []
	fun getSrc (A.OPER{assem=_, dst=_, src=src, jump=_}) = src
	  | getSrc (A.MOVE{assem=_, dst=_, src=src}) = [src]
	  | getSrc (A.LABEL{...}) = []
	fun contains (ls, item) = List.exists (fn x=> x=item) ls
					      
	fun updateUseDef (instr, {id= id, defs=defs, uses=uses}) =
	    let val dst = getDst instr
		val src = getSrc instr

			      
		(*for every src temp, add it to uses iff it hasn't been defined before*)
		val UseBeforeDef = List.filter (fn x => Bool.not(contains(defs, x))) src	   
	    in
		{id= id, defs= defs@dst, uses= uses@UseBeforeDef}(*for every dst temp, add it to defs anyway*)
	    end
		
	fun getUseDefOfBlock (id,bloc) = foldl updateUseDef {id=id, defs=[], uses=[]} bloc
	(*id -> Temp.temp list*)
	val allDefUses:{id: InstGraph.nodeID, defs:Temp.temp list, uses: Temp.temp list} list = map getUseDefOfBlock IDblockPairs

	(* val {id=id1, defs=defs1, uses=uses1} = List.nth(allDefUses, 1) *)
	(* val () = print("ID of node is:"^Int.toString(id1)) *)
	(* val () = print("\n Defs for this node are:  ") *)
	(* val () = printList(defs1) *)
	(* val () = print("\n Uses for this node are:  ") *)
	(* val () = printList(uses1) *)
	(* val () = print("\n") *)
	fun addUseToTable ({id=id, defs=defs, uses=uses}, (usetable, deftable)) = (UseDefTable.enter(usetable, id, uses)
										  ,UseDefTable.enter(deftable, id, defs))
	val (use, def) = foldl addUseToTable (UseDefTable.empty, UseDefTable.empty) allDefUses
	fun isMoveBlock ((instr as A.MOVE{...})::instrs)=true
	  | isMoveBlock _ = false
	fun addToIsMove ((id,bloc), ismovetable) = UseDefTable.enter(ismovetable, id, isMoveBlock(bloc))
	val ismove = foldl addToIsMove UseDefTable.empty IDblockPairs

    in (Flow.FGRAPH{control=finalGraph, def=def, use=use, ismove=ismove}, allNodes)
    end

end
    
	  
