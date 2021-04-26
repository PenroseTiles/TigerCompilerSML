structure Main = struct

   structure Tr = Translate
   structure F = MipsFrame

			      
   (*structure R = RegAlloc*)

 fun getsome (SOME x) = x

					     
 fun emitproc out (F.PROC{body,frame}) =
     let val _ = print ("emit " ^ Symbol.name (F.name frame) ^ "\n")
(*         val _ = Printtree.printtree(out,body); *)
	 val stms = Canon.linearize body
(*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	 val instrs =   List.concat(map (MipsGen.codegen frame) stms') 
         val format0 = Assem.format(F.makestring)
     in  app (fn i => TextIO.output(out,format0 i)) instrs;
	 instrs
     end
   | emitproc out (F.STRING(lab,s)) = let in (TextIO.output(out, s)); [] end

   fun withOpenFile fname f = 
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out) 
	    handle e => (TextIO.closeOut out; raise e)
       end 

   fun compile filename = 
       let val () = Translate.reset()
	   val absyn = Parse.parse filename
           val frags = (FindEscape.findEscape(absyn); Semant.transProg absyn)
	   val blocks = map (emitproc TextIO.stdOut) frags
	   val instrs = foldl (op @) [] blocks
	   val (flowgraph, fgnodes) = FlowGraphGenerator.instrs2graph (instrs)
	   val (Liveness.IGRAPH{graph =ig, tnode=tn, gtemp=gt, moves=m}, block2liveout)  = Liveness.interferenceGraph(flowgraph)
        in 
            (* withOpenFile (filename ^ ".s") f *)
	    Liveness.show(TextIO.stdOut, ig)
       end
  
end



