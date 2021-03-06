structure Main = struct

   structure Tr = Translate
   structure F = MipsFrame

			      
   (*structure R = RegAlloc*)

 fun getsome (SOME x) = x

					     
 fun emitproc out (F.PROC{body,frame}) =
     let val _ = TextIO.output(out, ".text\n")
	 val _ = TextIO.output(out, ".align 4\n")			      
	 val _ = TextIO.output(out, (Symbol.name(F.name frame))^":\n")
	 (*val _ = TextIO.output (out, "emit " ^ Symbol.name (F.name frame) ^ "\n")*)
	 
(*         val _ = Printtree.printtree(out,body); *)
	 val stms = Canon.linearize body
(*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	 val instrs =   List.concat(map (MipsGen.codegen frame) stms') 
         val format0 = Assem.format(F.makestring)
      in  app (fn i => TextIO.output(out,format0 i)) instrs
     end
   | emitproc out (F.STRING(lab,s)) = (TextIO.output(out, ".data\n");
				       TextIO.output(out, Symbol.name lab);
				       TextIO.output(out, ":\n.word "^Int.toString(String.size(s))^"\n"); 				       
				       TextIO.output(out, ".ascii ");
				       TextIO.output(out, "\""^s^"\"\n")				       
				      )

   fun withOpenFile fname f = 
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out) 
	    handle e => (TextIO.closeOut out; raise e)
       end 

   fun compile filename = 
       let val () = Translate.reset()
	   val absyn = Parse.parse filename
	   val irTree = Semant.transProg absyn

				      
           val frags = (FindEscape.findEscape(absyn); irTree)
        in 
            withOpenFile (filename ^ ".s") 
	     (fn out => (app (emitproc out) frags))
       end

end



