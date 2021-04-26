structure MipsFrame : FRAME =
struct
datatype access = InFrame of int | InReg of Temp.temp
datatype frame = Frame of {label: Temp.label, formals: access list, numAlloc: int ref}
val FP = Temp.newtemp()
val RV = Temp.newtemp()
val wordSize =4		     

fun exp (InFrame(k)) =
    let fun ret (fp: Tree.exp)=Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST k))
    in ret
    end
  | exp (InReg(k)) =
    let fun ret (fp: Tree.exp) = Tree.TEMP(k)
    in ret
    end	
	    
		  
fun newFrame ({formals=formals, name=name}) = let
    val numArgsProcessed = ref 0
    fun putFormals(b) = if b = true
			then (!numArgsProcessed = !numArgsProcessed + 1; InFrame(~4*(!numArgsProcessed)))
			else InReg(Temp.newtemp())
    fun createAccess() = map putFormals formals
in
    Frame{label=name, formals=createAccess(), numAlloc=numArgsProcessed}
end
							 

fun name (Frame{label=lab, formals=_, numAlloc=_}) = lab

fun formals (Frame{label=_, formals=forms, numAlloc=_}) = forms

fun allocLocal (fr as Frame{label=l, formals=f, numAlloc=numAlloc}) =
    let fun helper (Frame{label = l, formals = a::b , numAlloc=x}) = x := !x+1;
	fun ret escape =     if escape = true
		      then (helper(fr); InFrame(~4*(!numAlloc - 1)))
		      else InReg(Temp.newtemp())

    in
	ret
    end
	(* InFrame(0), InReg(t157), InReg(t158), sp <- sp-K, M[sp+K+0] <- r2, t157 <- r4, t158 <- r5 *)
end
    
