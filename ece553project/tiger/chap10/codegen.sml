signature CODEGEN =
sig
    structure Frame : FRAME
    val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen : CODEGEN =
struct
structure Frame = MipsFrame
structure A= Assem
structure T = Tree
structure Err=ErrorMsg
exception eexception
		  
fun codegen (frame) (stm: Tree.stm) : Assem.instr list =
    let val ilist = ref (nil : A.instr list)
	fun int x = if (x >= 0)
		    then Int.toString x
		    else "-"^Int.toString(~x)					 
	fun emit x = ilist :=x :: !ilist
	fun result (gen) = let val t = Temp.newtemp() in gen t; t end
	fun munchStm (T.MOVE(T.BINOP(T.PLUS, e1, T.CONST i), e2)) = (*e1 is a pointer and e2 contains the value to be moved
								     whereas in MIPS the value is the 1st arg and pointer is the 2nd*)
	    emit(A.OPER{assem = "sw `s0, "^int(i)^"(`s1)"^"\n",
			src=[munchExp e2, munchExp e1], dst=[], jump=NONE})
	  | munchStm (T.MOVE(T.BINOP(T.PLUS, T.CONST i, e1), e2)) =
	    emit(A.OPER{assem = "sw `s0, "^int(i)^"(`s1)\n",
			src=[munchExp e2, munchExp e1], dst=[], jump=NONE})
	  | munchStm (T.MOVE(T.MEM(e1), s as T.MEM(e2))) = 
	    emit(A.OPER{assem = "sw `s0, `s1\n",
			src=[munchExp s, munchExp e1], dst=[], jump=NONE})
	  | munchStm (T.MOVE(T.MEM(T.CONST i), e1)) =
	    emit(A.OPER{assem = "sw `s0 "^int(i)^"(r0)\n",
			src=[munchExp e1], dst=[], jump=NONE})
	  | munchStm (T.MOVE(target as T.MEM(e1), e2)) =
	    emit(A.OPER{assem = "sw `s0 `s1\n",
			src=[munchExp e2, munchExp target],dst=[],jump=NONE})
	  | munchStm (T.MOVE(T.TEMP i, e1))=  
	    emit(A.MOVE{assem = "move `d0, `s0\n",
			src=munchExp e1, dst=i})
	  | munchStm (T.LABEL label) = emit(A.LABEL{assem=Symbol.name label^"\n", lab=label})
	  | munchStm (T.JUMP(T.NAME(lab), labels)) =
	    emit(A.OPER{assem ="jump "^Symbol.name lab^"\n",

			src=[], dst=[], jump=SOME(labels)})
	  | munchStm (T.CJUMP(rel, e1, e2, lab1, lab2)) =
	    let fun relopToString (relop:T.relop): string = case relop of
							      T.EQ => "beq"
							    | T.NE => "bne"
							    | T.LT => "blt"
							    | T.GT => "bgt"
							    | T.LE => "ble"
							    | T.GE => "bge"
							    | T.ULT => "blt"
							    | T.ULE => "ble"
							    | T.UGT => "bgt"
							    | T.UGE => "bge"
	  in
	      
	    emit(A.OPER{assem=relopToString(rel)^" `s0, `s1 "^Symbol.name(lab1)^"\n",
			src=[munchExp e1, munchExp e2], dst=[], jump=SOME([lab1, lab2])})
	  end
							      
	  | munchStm (T.SEQ(stm1, stm2)) = let in  munchStm(stm1); munchStm(stm2) end
	  | munchStm (T.EXP(e1)) = (munchExp(e1);()) (*TODO *)
	  | munchStm _ = ( Err.error 1 "Could not generate assembly";raise eexception)
	and  munchExp(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i)))=
	    result(fn r => emit(A.OPER
				    {assem = "lw `d0, " ^int(i)^"(`s0)" ^ "\n",
				     src=[munchExp e1], dst=[r], jump=NONE}))
	  | munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) =
	    result(fn r=> emit(A.OPER
				   {assem = "lw `d0, "^int(i)^"(`s0)" ^ "\n",
				    src = [munchExp e1], dst=[r], jump=NONE}))
	  | munchExp (T.MEM(T.BINOP(T.MINUS, e1, T.CONST i))) =
	    result(fn r=> emit(A.OPER
				   {assem = "lw `d0, " ^ int (~i) ^"(`s0)" ^ "\n",
				    src = [munchExp e1], dst=[r], jump=NONE}))
	  | munchExp (T.MEM(T.CONST i)) =
	    result(fn r => emit(A.OPER
				    {assem = "lw `d0, " ^ int(i) ^ "(`r0)"^"\n",
				     src=[], dst=[r], jump=NONE}))
	  | munchExp (T.MEM(e1)) = 
	    result(fn r => emit(A.OPER
				    {assem = "lw `d0, 0(`s0)\n",
				     src=[munchExp e1], dst=[r], jump=NONE}))
	  | munchExp (T.BINOP(T.PLUS, e1, T.CONST i)) =
	    result(fn r => emit(A.OPER
				    {assem = "addi `d0, `s0, " ^ int(i) ^ "\n",
				     src = [munchExp e1], dst= [r], jump=NONE}))
	  | munchExp (T.BINOP(T.PLUS, T.CONST i, e1)) =
	    result (fn r => emit(A.OPER
				     {assem="addi `d0, `s0, " ^ int(i) ^ "\n",
				      src = [munchExp e1], dst = [r], jump=NONE}))
	  | munchExp (T.CONST i) =
	    result(fn r=> emit(A.OPER
				   {assem = "addi `d0, r0," ^ int(i) ^ "\n",
				    src=[], dst= [r], jump=NONE}))
	  | munchExp (T.BINOP(T.PLUS, e1, e2)) =
	    result(fn r=> emit(A.OPER
				   {assem = "add `d0, `s0, `s1\n",
				    src = [munchExp e1, munchExp e2], dst= [r],
				    jump=NONE}))
	  | munchExp (T.BINOP(T.MINUS, e1, T.CONST i)) =
	    result(fn r => emit (A.OPER
				     {assem = "addi `d0, `s0, " ^ int (~i) ^ "\n",
				      src = [munchExp e1], dst = [r], jump=NONE}))
	  | munchExp (T.BINOP(T.MINUS, e1, e2)) =
	    result(fn r => emit (A.OPER
				     {assem = "addi `d0, `s0, `s1 \n",
				      src = [munchExp e1, munchExp e2], dst = [r], jump=NONE}))
	  | munchExp (T.BINOP(T.MUL, e1, e2)) = 
	    result(fn r => emit (A.OPER
				     {assem = "mult `d0, `s0, `s1 \n",
				      src = [munchExp e1, munchExp e2], dst = [r], jump=NONE}))
	  | munchExp (T.BINOP(T.DIV, e1, e2)) = 
	    result(fn r => emit (A.OPER
				     {assem = "div `d0, `s0, `s1 \n",
				      src = [munchExp e1, munchExp e2], dst = [r], jump=NONE}))
	  | munchExp (T.BINOP(T.AND, e1, e2)) = 
	    result(fn r => emit (A.OPER
				     {assem = "and `d0, `s0, `s1 \n",
				      src = [munchExp e1, munchExp e2], dst = [r], jump=NONE}))
(*	  | munchExp (T.BINOP(T.AND, e1, T.CONST i)) = 
	    result(fn r => emit (A.OPER
				     {assem = "andi 'd0, 's0, " ^ int i ^ " \n",
				      src = [munchExp e1], dst = [r], jump=NONE}))
	  | munchExp (T.BINOP(T.AND, T.CONST i, e1)) = 
	    result(fn r => emit (A.OPER
				     {assem = "andi 'd0, 's0, " ^ int i ^ " \n",
				      src = [munchExp e1], dst = [r], jump=NONE}))
*)	  | munchExp (T.BINOP(T.OR, e1, e2)) = 
	    result(fn r => emit (A.OPER
				     {assem = "or `d0, `s0, `s1 \n",
				      src = [munchExp e1, munchExp e2], dst = [r], jump=NONE}))
(*	  | munchExp (T.BINOP(T.OR, e1, T.CONST i)) = 
	    result(fn r => emit (A.OPER
				     {assem = "ori 'd0, 's0, " ^ int i ^ " \n",
				      src = [munchExp e1], dst = [r], jump=NONE}))
	  | munchExp (T.BINOP(T.OR, T.CONST i, e1)) = 
	    result(fn r => emit (A.OPER
				     {assem = "ori 'd0, 's0, " ^ int i ^ " \n",
				      src = [munchExp e1], dst = [r], jump=NONE}))
*)	  | munchExp (T.BINOP(T.LSHIFT, T.CONST i, e1)) = 
	    result(fn r => emit (A.OPER
				     {assem = "sll `d0, `s0, " ^ int(i) ^ " \n",
				      src = [munchExp e1], dst = [r], jump=NONE}))
	  | munchExp (T.BINOP(T.RSHIFT, T.CONST i, e1)) = 
	    result(fn r => emit (A.OPER
				     {assem = "srl `d0, `s0, " ^ int(i) ^ " \n",
				      src = [munchExp e1], dst = [r], jump=NONE}))
	  | munchExp (T.BINOP(T.ARSHIFT, T.CONST i, e1)) = 
	    result(fn r => emit (A.OPER
				     {assem = "sra `d0, `s0, " ^ int(i) ^ " \n",
				      src = [munchExp e1], dst = [r], jump=NONE}))
	  | munchExp (T.BINOP(T.XOR, e1, e2)) = 
	    result(fn r => emit (A.OPER
				     {assem = "xor `d0, `s0, `s1 \n",
				      src = [munchExp e1, munchExp e2], dst = [r], jump=NONE}))
	  | munchExp (T.TEMP t)  = t
	  | munchExp (T.ESEQ(stm, e)) = (munchStm stm; munchExp e)
	  | munchExp (T.NAME(label)) =
	    result(fn r => emit(
			      A.OPER {assem = "la `d0, " ^ Symbol.name(label) ^ "\n",
				      src=[], dst= [r], jump=NONE}))
	
	  | munchExp (T.CALL(T.NAME(name), arguments)) =
	    
	    let
		val returnaddr = Frame.RA
		val returnval = Frame.RV
		val calldefs = (returnaddr :: Frame.callersaves) @ [returnval]
		     in
			 emit(A.OPER{assem = "jal " ^ Symbol.name(name) ^ "\n",
				     src = let in munchArgs(0, arguments, 16); [] end, dst = calldefs, jump =NONE});
			 returnval (*return return value after callexp*)
		     end
	  | munchExp _ = ( Err.error 1 "Couldn't generate assembly";raise eexception)
	and munchArgs(i, [], offset) = [] (*check if there are more than 4 arguments. there are 4 argument registers in MIPS so if there are more than 4, put them on the Frame*)
					  
	    | munchArgs (numArgsPlaced, arg::argList, frameOffset) =
	      let
		  fun getArgRegLeft (i: int) : Temp.temp = case i of 0 => Frame.A0
								   | 1 => Frame.A1
								   | 2 => Frame.A2
								   | 3 => Frame.A3
								   | _ =>  raise eexception	       
		  fun argInTemp arg = munchStm(T.MOVE(T.TEMP(getArgRegLeft(numArgsPlaced)), arg)) 
		  fun argInFrame (arg, frameOffset)= munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP(Frame.SP), T.CONST frameOffset)), arg))
	      in
		  if numArgsPlaced<4
		  then (argInTemp(arg)::munchArgs(numArgsPlaced+1, argList, frameOffset)) (*only iterate offset if something is stored on frame*)
		  else (argInFrame(arg, frameOffset)::munchArgs(numArgsPlaced+1, argList, frameOffset+4))
	      end
	  in munchStm stm;
	     rev(!ilist)
	  end
end
