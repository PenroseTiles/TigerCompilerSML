signature REG_ALLOC =
sig
    structure Frame: FRAME
    type allocation = Frame.register Temp.Table.table
    val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end
    
signature COLOR =
sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Table.table
    val color: {interference: Temp.temp FG.graph,
		initial: allocation,
		spillCost: Temp.temp FG.node -> int,
		registers: Frame.register list} -> allocation * Temp.temp list
end


structure RegAllocColor:COLOR =
struct
structure Frame = MipsFrame
type allocation = Frame.register Temp.Table.table
exception NotEnoughRegisters
fun color ({interference=igraph,
	    initial= init_alloc,
	    spillCost = spillcost,
	    registers=registers}) =
    let 
	val numcolors = 23 (*TODO get this from MipsFrame*)
	fun printList([])= print("\n")
	  | printList (x::xs) = (print(Int.toString(x));print(", ");printList(xs))
	val alltempsinigraph = map FG.nodeInfo (FG.nodes(igraph))
	(* val () = print("These are the temps in igraph:\n") *)
	(* val () = printList(alltempsinigraph) *)
	fun simplify (stack, igraph) =	    
	    let 
		val alltempnodes:Temp.temp FG.node list = FG.nodes(igraph)
		fun printTempAndDegree (node) = (print("This is the node id: "^(Int.toString(FG.getNodeID(node)))^" which has degree "^Int.toString(FG.degree(node))^"\n");
						 print("These are the adjacent nodes: ");
						 printList(FG.adj(node));
						 print("\n"))
		(*val () = app printTempAndDegree alltempnodes*)
		fun isNonsignificantTemp (tempnode) = ((FG.degree(tempnode)) div 2)<numcolors
		fun notPrecolored tempnode = case Temp.Table.look(init_alloc, FG.getNodeID(tempnode)) of
						 NONE => true
					       | _ => false
						       
		fun checkNonsignificantTemps [] = NONE
		  | checkNonsignificantTemps (node::templist) = if (isNonsignificantTemp(node) andalso notPrecolored(node)) then SOME(node)
								else checkNonsignificantTemps(templist)		
	   	val nodeToEliminate = checkNonsignificantTemps alltempnodes
	    in
		case nodeToEliminate of NONE => (stack, igraph)
				      | SOME(x) => simplify(x::stack, FG.removeNode(igraph, FG.getNodeID(x)))
	    end
		
	fun contains(item, [])=false
	  | contains (item, x::xs) = if item=x then true else contains(item, xs)
								      
	fun select ([], igraph, allocation) =
	    let val allnodes = FG.nodes(igraph)
		fun isNotColored nodeID = case Temp.Table.look(allocation, nodeID) of SOME(x)=> false
										    | NONE => true
		val uncoloredNodes = List.filter (isNotColored o FG.getNodeID) allnodes
	    in
		if length(uncoloredNodes)>0
		then raise NotEnoughRegisters
		else (igraph, allocation) (*TODO fix this*)
	    end
		
	 |  select (node::stack, igraph, allocation) =
	    let	val stackinfo = map FG.nodeInfo (FG.nodes(igraph))
		(* val () = print("These are the temps in the simplified graph:\n") *)
		(* val () = printList(stackinfo)		     *)
		fun findUsedRegisters (node) =
		    let val neighbors = FG.adj(node)
			val neighboringRegisters:Frame.register list = List.mapPartial
						       (fn node => Temp.Table.look(allocation, node))
						       neighbors
						    
		    in neighboringRegisters
		    end
		val neighboringregs:Frame.register list = findUsedRegisters(node) 
		fun pickNewRegister (neighboringregs, reg::remainingRegs) = if Bool.not (contains(reg, neighboringregs))
									    then reg
									    else pickNewRegister(neighboringregs, remainingRegs)
		
		val newreg:Frame.register = pickNewRegister(neighboringregs, Frame.colorableRegisters)
		val nodeid= FG.getNodeID(node)
		val newgraph = FG.addNode(igraph, nodeid, FG.nodeInfo(node)) (*need to add the node back along with all its edges*)
		val edgesToAdd = FG.adj(node) 			 
		val newgraphwithedges = foldl (fn (otherid,ig) => FG.doubleEdge(ig, nodeid, otherid)) newgraph edgesToAdd
		val newallocation = Temp.Table.enter(allocation, FG.nodeInfo(node), newreg)
	    in
		select(stack, newgraph, newallocation)
	    end
	val (simplifiednodes, simplifiedgraph) = simplify([], igraph)
	val (igraphsel, allocsel) = select(simplifiednodes, simplifiedgraph, init_alloc)
    in
	(allocsel,[])
    end	
end

structure MipsRegAlloc:REG_ALLOC=
struct
structure Frame=MipsFrame
structure Liveness=Liveness
structure MakeGraphGenerator= FlowGraphGenerator
type allocation = Frame.register Temp.Table.table
fun printAlloc(alloc) =
    let val alltemps = Temp.Table.listKeys(alloc)
	fun printreg({name=n, label=t}) = print("{name="^n^", label="^Int.toString(t)^"}\n ")
	fun printMapping(temp) = (print(Int.toString(temp)^" -> ");printreg(valOf(Temp.Table.look(alloc, temp))))
    in
	app printMapping alltemps
    end
	
	
fun alloc (inslist, frame) =
    let val (flowgraph, allblocks) = MakeGraphGenerator.instrs2graph(inslist)
	val (Liveness.IGRAPH{graph =igraph, tnode=tn, gtemp=gt, moves=m}, block2liveout) = Liveness.interferenceGraph(flowgraph)													     
	val (alloc, stack) = RegAllocColor.color({interference=igraph,
						  initial=Frame.tempToRegisterMap,spillCost= (fn x=>1),
						  registers =Frame.colorableRegisters})
    in (*printAlloc(alloc);*)
       (inslist, alloc)
    end
	
end
