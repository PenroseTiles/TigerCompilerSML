structure MipsFrame : FRAME =
struct
datatype access = InFrame of int | InReg of Temp.temp
type frame = {label: Temp.label, formals: access list, numAlloc: int ref}

datatype frag = PROC of {body: Tree.stm, frame: frame}
	      | STRING of Temp.label * string

type register = {name: string, label: Temp.temp}

		    
val FP = Temp.newtemp()
val RV = Temp.newtemp()
val RA = Temp.newtemp()
val SP = Temp.newtemp()
val ZERO = Temp.newtemp()
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
val specialregs = [RA, SP, ZERO, V0, V1, GP, FP]
val argregs = [A0, A1, A2, A3]
val callersaves = [T0, T1, T2, T3, T4, T5, T6, T7, T8, T9]
val calleesaves = [S0, S1, S2, S3, S4, S5, S6, S7]

fun getCalleeSaves() = calleesaves
fun getCallerSaves() = callersaves
fun getSpecialRegs() = specialregs (*TODO check if order of these registers is as expected*)
fun getArgRegs() = argregs
val regRecord = [(FP, "fp"), (RA, "ra"), (ZERO, "zero"),(SP, "sp"), (GP, "gp"), (V0, "v0"), (V1, "v1"),
		 (A0, "a0"), (A1, "a1"), (A2, "a2"), (A3, "a3"), (S0, "s0"), (RV, "rv"),
		 (S1, "s1"), (S2, "s2"), (S3, "s3"), (S4, "s4") , (S5, "s5"), (S6, "s6"), (S7, "s7"),
		(T0, "t0"), (T1, "t1"), (T2, "t2"), (T3, "t3"), (T4, "t4") , (T5, "t5"), (T6, "t6"), (T7, "t7"),(T8, "t8"), (T9, "t9")]


val tempMap = let 
		  fun addToTable ((t,s), table) = Temp.Table.enter(table, t, s)
	      in
		  foldl addToTable Temp.Table.empty regRecord
	      end
		  
fun makestring (temp) = let val x = Temp.Table.look(tempMap, temp)
			  in
			     
			     (case x of SOME(string) => string
				      | NONE => Temp.makestring(temp))
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
in
   {label = name, formals= createAccess(), numAlloc= numArgsProcessed}
end
							 

fun name(frameArg:frame) = #label frameArg

fun formals(frameArg:frame) = #formals frameArg

fun allocLocal ({label, formals, numAlloc})(escape) =
    case escape of true => (numAlloc := !numAlloc + 1; InFrame(0 - (4 * !numAlloc)))
		| false => InReg(Temp.newtemp())

fun exp (accessArg, frameArg) =
    (case accessArg of InFrame offsetArg => Tree.MEM(Tree.BINOP(Tree.PLUS, frameArg, Tree.CONST(offsetArg)))
		    | InReg tempArg => Tree.TEMP(tempArg))
						     
fun procEntryExit1 (frame, body) = body

fun procEntryExit2 (frame, body) =
    body @ [Assem.OPER{assem="", src=(([ZERO, RA, SP]@calleesaves)), dst=[], jump=SOME[]}]

fun procEntryExit3({label=name, formals=params, numAlloc=locals}, body) =
    {prolog = "PROCEDURE " ^ Symbol.name (name) ^ "\n",
     body = body,
     epilog = "END " ^ Symbol.name (name) ^ "\n"}
	
fun externalCall (s, args) =
    Tree.CALL(Tree.NAME(Temp.namedlabel s), args)
end


(* InFrame(0), InReg(t157), InReg(t158), sp <- sp-K, M[sp+K+0] <- r2, t157 <- r4, t158 <- r5 *)
					

    
