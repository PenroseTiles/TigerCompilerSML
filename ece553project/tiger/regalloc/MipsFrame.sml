structure MipsFrame : FRAME =
struct
datatype access = InFrame of int | InReg of Temp.temp
type frame = {viewShift: Tree.stm list, label: Temp.label, formals: access list, numAlloc: int ref}

datatype frag = PROC of {body: Tree.stm, frame: frame}
	      | STRING of Temp.label * string

type register = {name: string, label: Temp.temp}
exception eexception

fun getNameFromFrag (PROC{body=body, frame=f}):string = Symbol.name (#label f)
  | getNameFromFrag  (STRING(lab, str)):string = Symbol.name lab

fun isStringFrag (PROC{body=_, frame=_}) = false
  | isStringFrag _ = true	       

fun getNamesFromFragPair (frag1, frag2) = (getNameFromFrag(frag1),getNameFromFrag(frag2))
					      
structure fragSet = RedBlackSetFn(struct
				type ord_key = frag
				val compare = String.compare o (getNamesFromFragPair)
				end)

fun addFragsToSet (fraglist):frag list =
    let fun addfrag(frag, fragset)=fragSet.add(fragset, frag)
	val fragsUnique = foldl addfrag fragSet.empty fraglist
    in fragSet.toList(fragsUnique)
    end
	

fun seq ([s1]) = s1
  | seq ([s1,s2]) = Tree.SEQ(s1, s2)
  | seq (s1::seqlist)= Tree.SEQ(s1, seq(seqlist))
  | seq [] = Tree.EXP(Tree.CONST 0 )
		    
val FP = Temp.newtemp()
val RV = Temp.newtemp()
val RA = Temp.newtemp()
val SP = Temp.newtemp()
val R0 = Temp.newtemp()
val GP = Temp.newtemp()
		     
val A1 = Temp.newtemp()
val A2 = Temp.newtemp()		     
val A3 = Temp.newtemp()
val A0 = Temp.newtemp()
		     
val T0 = Temp.newtemp()
val T1 = Temp.newtemp()
val T2 = Temp.newtemp()
val T3 = Temp.newtemp()
val T4 = Temp.newtemp()
val T5 = Temp.newtemp()
val T6 = Temp.newtemp()
val T7 = Temp.newtemp()
val T8 = Temp.newtemp()		     
val T9 = Temp.newtemp()
		     
val S0 = Temp.newtemp()
val S1 = Temp.newtemp()
val S2 = Temp.newtemp()
val S3 = Temp.newtemp()
val S4 = Temp.newtemp()
val S5 = Temp.newtemp()
val S6 = Temp.newtemp()
val S7 = Temp.newtemp()		     
		     
val V0 = Temp.newtemp()
val V1 = Temp.newtemp()		     

		     
val wordSize = 4
val specialregs = [RA, SP, R0, V0, V1, GP, FP]
val argregs = [A0, A1, A2, A3]
val callersaves = [T0, T1, T2, T3, T4, T5, T6, T7, T8, T9]
val calleesaves = [S0, S1, S2, S3, S4, S5, S6, S7]

fun getCalleeSaves() = calleesaves
fun getCallerSaves() = callersaves
fun getSpecialRegs() = specialregs (*TODO check if order of these registers is as expected*)
fun getArgRegs() = argregs
val regRecord = [(FP, "fp"), (RA, "ra"), (R0, "zero"),(SP, "sp"), (GP, "gp"), (V0, "v0"), (V1, "v1"),
		 (A0, "a0"), (A1, "a1"), (A2, "a2"), (A3, "a3"), (S0, "s0"), (RV, "v0"),
		 (S1, "s1"), (S2, "s2"), (S3, "s3"), (S4, "s4") , (S5, "s5"), (S6, "s6"), (S7, "s7"),
		(T0, "t0"), (T1, "t1"), (T2, "t2"), (T3, "t3"), (T4, "t4") , (T5, "t5"), (T6, "t6"), (T7, "t7"),(T8, "t8"), (T9, "t9")]

		    


val tempMap = let 
		  fun addToTable ((t,s), table) = Temp.Table.enter(table, t, s)
	      in
		  foldl addToTable Temp.Table.empty regRecord
	      end

val tempToRegisterMap:register Temp.Table.table = let
    fun addToTable ((t,s), table) = Temp.Table.enter(table, t, {name=s, label=t})
in
		  foldl addToTable Temp.Table.empty regRecord
end

fun tempToReg t	= valOf(Temp.Table.look(tempToRegisterMap,t))


val colorableRegisters:register list = map tempToReg (calleesaves@callersaves) (*TODO fix types*)
						      
fun makestring (temp) = let val x = Temp.Table.look(tempMap, temp)
			  in
			     
			     (case x of SOME(string) => string
				      | NONE => Temp.makestring(temp))
			 end

fun allocationToString (allocation)(temp:Temp.temp) =
    let val {name=name, label=_} = valOf(Temp.Table.look(allocation, temp))
    in name
    end

fun zip([],[])=[]
  | zip (x::xs, y::ys) = (x,y)::zip(xs, ys)

fun exp (accessArg, frameArg) =
    (case accessArg of InFrame offsetArg => Tree.MEM(Tree.BINOP(Tree.PLUS, frameArg, Tree.CONST(offsetArg)))
		    | InReg tempArg => Tree.TEMP(tempArg))

fun take(ls, 0)=[]
  | take (x::xs, i) = x::take(xs, i-1)
  | take ([],i) =[] 
				   
							  
fun printaccess acc =
    let
    in
	case acc of
	    InFrame(k) => print("InFrame at "^Int.toString(k)^"\n")
	 |  InReg(k) => print("InReg at "^Int.toString(k)^"\n")
    end			 


fun newFrame {name: Temp.label, formals: bool list} = let
    val numArgsProcessed = ref 0
    fun putFormals(b) =
	let
	in
	    case b of 
		true => ((!numArgsProcessed = !numArgsProcessed + 1) ; InFrame(0-(4*(!numArgsProcessed))))
	      | false => InReg(Temp.newtemp())
	end  
    fun createAccess() = map putFormals formals
    val accesses = createAccess()
    (*val _ = print("In newFrame\n")
    val _ = app (print o Bool.toString) formals
    val _ = print("\n")
    val _ = app printaccess accesses*)

		
    fun indexToAreg 0 = A0
      | indexToAreg 1 = A1
      | indexToAreg 2 = A2
      | indexToAreg 3 = A3
      | indexToAreg _ = raise eexception
    val firstFour = take(accesses, 4)
    val indices = List.tabulate(length(firstFour), fn x => x)
    val argToViewShift = zip(firstFour, indices)
    fun f (acc, i) = Tree.MOVE(exp(List.nth(accesses, i), Tree.TEMP FP), Tree.TEMP (indexToAreg(i))		     )
    val viewShift = map f argToViewShift
in
   {viewShift = viewShift, label = name, formals= accesses, numAlloc= numArgsProcessed}
end
		     

fun name(frameArg:frame) = #label frameArg

fun formals(frameArg:frame) = #formals frameArg

fun viewshift(frameArg:frame) = #viewShift frameArg
				       
fun allocLocal ({label, formals, numAlloc, viewShift}:frame)(escape) =
    case escape of true => (numAlloc := !numAlloc + 1; InFrame(0 - (4 * (!(numAlloc)+1))))
		| false => InReg(Temp.newtemp())

					       
(* fun removeDuplicateFunctions([]) = [] *)
(*   | removeDuplicateFunctions (frag::frags) = *)
(*     let val isString = isStringFrag(frag) *)
				   
(*     in if isString then frag::frags *)
(*     end *)
	
					       
fun procEntryExit1 (frame:frame,
		    frametemp, body) =
    let val escape = true
	val vs = viewshift(frame)

	val oldFP = allocLocal(frame)(escape)
	val oldFPs = Tree.MOVE(Tree.TEMP(FP), exp(oldFP, frametemp))		    
	val access0 = allocLocal(frame)(escape)
	val irs0 = Tree.MOVE(Tree.TEMP(S0), exp(access0, frametemp))
	val access1 = allocLocal(frame)(escape)
	val irs1 = Tree.MOVE(Tree.TEMP S1, exp(access1, frametemp))
	val access2 = allocLocal(frame)(escape)
	val irs2 = Tree.MOVE(Tree.TEMP S2, exp(access2, frametemp))
	val access3 = allocLocal(frame)(escape)
	val irs3 = Tree.MOVE(Tree.TEMP S3, exp(access3, frametemp))
	val access4 = allocLocal(frame)(escape)
	val irs4 = Tree.MOVE(Tree.TEMP S4, exp(access4, frametemp))
	val access5 = allocLocal(frame)(escape)
	val irs5 = Tree.MOVE(Tree.TEMP S5, exp(access5, frametemp))
	val access6 = allocLocal(frame)(escape)
	val irs6 = Tree.MOVE(Tree.TEMP S6, exp(access6, frametemp))
	val access7 = allocLocal(frame)(escape)
	val irs7 = Tree.MOVE(Tree.TEMP S7, exp(access7, frametemp))
	val accessRA = allocLocal(frame)(escape)
	val irRA = Tree.MOVE(Tree.TEMP RA, exp(accessRA, frametemp))
			    
	val allIRrestores  = seq(irs0::irs1::irs2::irs3::irs4::irs5::irs6::irs7::[irRA])
	val IRrestoreoldFP =  Tree.MOVE(exp(oldFP, frametemp), Tree.TEMP(FP))
	val IRrestoreS0 = Tree.MOVE(exp(access0, frametemp), Tree.TEMP(S0))
	val IRrestoreS1 = Tree.MOVE(exp(access1, frametemp), Tree.TEMP(S1))
	val IRrestoreS2 = Tree.MOVE(exp(access2, frametemp), Tree.TEMP(S2))
	val IRrestoreS3 = Tree.MOVE(exp(access3, frametemp), Tree.TEMP S3)
	val IRrestoreS4 = Tree.MOVE(exp(access4, frametemp), Tree.TEMP S4)
	val IRrestoreS5 = Tree.MOVE(exp(access5, frametemp), Tree.TEMP S5)
	val IRrestoreS6 = Tree.MOVE(exp(access6, frametemp), Tree.TEMP S6)
	val IRrestoreS7 = Tree.MOVE(exp(access7, frametemp), Tree.TEMP S7)
	val irRArestore = Tree.MOVE(exp(accessRA, frametemp), Tree.TEMP RA)
	val allIRsaves =
	    seq([IRrestoreS0, IRrestoreS1, IRrestoreS2, IRrestoreS3, IRrestoreS4, IRrestoreS5, IRrestoreS6, IRrestoreS7, irRArestore])
		
    in
	seq([allIRsaves, seq([seq(vs),body]), allIRrestores])
    end
				       

fun procEntryExit2 (frame, body) =
    body @ [Assem.OPER{assem="", src=(([R0, RA, SP]@calleesaves)), dst=[], jump=SOME[]}]

fun procEntryExit3 ({label=name, formals=params, numAlloc=locals, viewShift=viewShift}, body, offset) =
    let	
	val incrementSPinstruction = Assem.OPER{assem = "addi $`d0, $`s0, -" ^ Int.toString((offset+2)*4) ^ "\n", (*change sp*)
						src = [SP], dst = [SP], jump=NONE}
	val SPtoFPinstruction = Assem.OPER{assem = "sw $`d0, "^Int.toString((offset-1)*4)^"($`s0)\n",
					   src = [SP], dst = [FP], jump=NONE} (*sw fp --> -4(sp)*)
	val addFPinstruction = Assem.OPER{assem = "addi $`d0, $`s0, "^Int.toString((offset)*4)^"\n",
					  src = [SP], dst = [FP], jump=NONE}

	
	val loadFPinstruction = Assem.OPER{assem = "lw $`d0, "^Int.toString((offset-1)*4)^"($`s0)\n",
					   src = [SP], dst = [FP], jump=NONE} (*lw fp <-- -4(fp)*)
	val moveSPinstruction2 = Assem.OPER{assem = "addi $`d0, $`s0, "^Int.toString((offset+2)*4)^"\n",
					    src = [SP], dst = [SP], jump=NONE} (*move sp <-- fp*)
	val returnInstruction = Assem.OPER{assem = "jr $`d0\n",
					   src = [], dst = [RA], jump=NONE} (*jr ra*)

    in
	{prolog = "PROCEDURE " ^ Symbol.name (name) ^ "\n",
	 body = ([incrementSPinstruction]
		 @[SPtoFPinstruction]
		 @[addFPinstruction]
		 @body
		 @[loadFPinstruction]
		 @[moveSPinstruction2]
		 @[returnInstruction]),
	 epilog = "END " ^ Symbol.name (name) ^ "\n"}
    end
	
fun externalCall (s, args) =
    Tree.CALL(Tree.NAME(Temp.namedlabel s), args)

end

(* InFrame(0), InReg(t157), InReg(t158), sp <- sp-K, M[sp+K+0] <- r2, t157 <- r4, t158 <- r5 *)
					

    
