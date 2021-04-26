structure Main = struct

   structure Tr = Translate
   structure F = MipsFrame

			      
   (*structure R = RegAlloc*)

 fun getsome (SOME x) = x

					     
 fun emitproc out (F.PROC{body,frame}) =
     let			 
	 val _ = TextIO.output(out, ".text\n")
	 val _ = TextIO.output(out, ".align 4\n")			      
	 val _ = TextIO.output(out, (Symbol.name(F.name frame))^":\n")
(*         val _ = Printtree.printtree(out,body); *)
	 val stms = Canon.linearize body
(*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	 val instrs =   List.concat(map (MipsGen.codegen frame) stms')
	 val instrs' = F.procEntryExit2(frame, instrs)
				       
	  (*val format1 = Assem.format(F.makestring) 
	  val () = app (fn i => TextIO.output(TextIO.stdOut,format1 i)) instrs *)


	 val (_, regalloc) = MipsRegAlloc.alloc(instrs, frame)
         val format0 = Assem.format(F.allocationToString(regalloc))
	 fun getOffset frags =
	     let
		 fun findMaxforFrag (F.STRING(_,_) , offset) = offset
		   | findMaxforFrag (F.PROC({body, frame}), offset) =Int.max(offset, !(#numAlloc frame))
									    
	     in
		 foldl findMaxforFrag 0 frags
	     end
		 
	 val offset = getOffset(Tr.getResult())
	 val finalinstrs = F.procEntryExit3(frame, instrs, offset)
					   
     in  app (fn i => TextIO.output(out,format0 i)) (#body finalinstrs);
	 instrs
     end
   | emitproc out (F.STRING(lab,s)) = (TextIO.output(out, ".data\n");
				       TextIO.output(out, Symbol.name lab^":");
				       TextIO.output(out, "\n.word "^Int.toString(String.size(s))^"\n"); 				       
				       TextIO.output(out, ".ascii ");
				       TextIO.output(out, "\""^s^"\"\n");[]				       
				      )	 

   fun withOpenFile fname f = 
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out) 
	    handle e => (TextIO.closeOut out; raise e)
       end 

   fun compile filename = 
       let val () = Translate.reset()
	   val absyn = Parse.parse filename
           val frags = (FindEscape.findEscape(absyn); Semant.transProg absyn)
	   val f = TextIO.openOut (filename^".s")
	   val ins = TextIO.openIn("runtime-le.s")
	   val text = TextIO.inputAll(ins)
	   val _ = TextIO.output(f, text)
	   val _ = TextIO.closeIn(ins)
	   val ins2 = TextIO.openIn("sysspim.s")
	   val text2 = TextIO.inputAll(ins2)
	   val uniquefrags = F.addFragsToSet(frags) 

	    (*val _ = app Tr.printonefrag uniquefrags *)

	   val blocks = map (emitproc f) uniquefrags
	   val _ = TextIO.output(f,text2)
	   val _ = TextIO.closeIn(ins2)
	   val () = TextIO.closeOut f
	   (*val instrs = foldl (op @) [] blocks
	   val (flowgraph, fgnodes) = FlowGraphGenerator.instrs2graph (instrs)
	   val (Liveness.IGRAPH{graph =ig, tnode=tn, gtemp=gt, moves=m}, block2liveout)  = Liveness.interferenceGraph(flowgraph)*)
        in 
            (*withOpenFile (filename ^ ".s") (fn out => (app (emitproc out) frags));*)
	    ()(*Liveness.show(TextIO.stdOut, ig)*)
       end
  
end



