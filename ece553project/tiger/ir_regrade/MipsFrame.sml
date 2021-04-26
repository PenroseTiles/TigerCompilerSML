structure MipsFrame : FRAME =
struct
datatype access = InFrame of int | InReg of Temp.temp
type frame = {label: Temp.label, formals: access list, numAlloc: int ref}

datatype frag = PROC of {body: Tree.stm, frame: frame}
	      | STRING of Temp.label * string
val FP = Temp.newtemp()
val RV = Temp.newtemp()
val wordSize = 4
fun newFrame{name: Temp.label, formals: bool list} = let
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

fun exp(accessArg, frameArg) =
    (case accessArg of InFrame offsetArg => Tree.MEM(Tree.BINOP(Tree.PLUS, frameArg, Tree.CONST(offsetArg)))
		    | InReg tempArg => Tree.TEMP(tempArg))
						     
fun procEntryExit1(frame, body) = body
				    
fun externalCall(s, args) =
    Tree.CALL(Tree.NAME(Temp.namedlabel s), args)
end


(* InFrame(0), InReg(t157), InReg(t158), sp <- sp-K, M[sp+K+0] <- r2, t157 <- r4, t158 <- r5 *)
					

    
