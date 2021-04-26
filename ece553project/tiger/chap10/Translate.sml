(*need to do strings on pg 163*)
(*add break argument to TransExp and transDec on pg 166*)
signature TRANSLATE =
sig
    type level
(*is level a tree.const?*)
    type access
    datatype exp = Ex of Tree.exp
		 | Nx of Tree.stm
		 | Cx of Temp.label * Temp.label -> Tree.stm
    type frag  
    val outermost: level (*level within which the a program is nested*)
    val newLevel: {parent:level, name:Temp.label, formals: bool list} -> level
									      
    val formals: level -> access list
    val allocLocal: level * bool -> level * MipsFrame.access
				       
    val transAssign: (exp * exp) -> exp
    val transRelop : (Types.ty * Tree.relop * exp *exp) -> exp
    (*val transLet: () ->*) (*THIS IS NOT BEING USED RIGHT NOW --- why?*)
    val translateArithmetic : (Tree.binop * exp * exp) -> exp
    val translateIf: (exp * exp * exp) -> exp
    val translateIfNoElse : (exp*exp)->exp					   
    val	transNilExp : unit -> exp
						 
    val transMkRec: (exp list) -> exp
    val transMkArray: (exp * exp ) -> exp
    val transWhile: (exp * exp * Temp.label) -> exp
    val transFor: (exp * exp * exp * exp * Tree.label) -> exp
							   
    val transBreak: (Tree.label) -> exp
    val seq: (Tree.stm list) -> Tree.stm
    val transFieldVar: (exp * int) -> exp 
    val transSubscriptVar: (exp * exp) -> exp
    val getFrame: (level * level) -> exp
    val simpleVar: ((level * MipsFrame.access) * level) -> exp
    val transSTRING: string -> exp
    val printonefrag: MipsFrame.frag -> unit
    val reset: unit -> unit
		      
    (*val callExp: (Tree.CONST *  Tree.CONST * Temp.label * Absyn.exp list) -> Tree.exp	*)
    val callExp: (level * level * Tree.label * exp list) -> exp
    val transLet: exp list * exp -> exp								  
    val procEntryExit : {level: level, body: exp} -> unit
    val transSeq: exp list -> exp
    val	printTreeHelper: exp -> unit				      
    val getResult : unit -> MipsFrame.frag list
    val frameFromLevel: level -> MipsFrame.frame
    val printLevel: level -> unit				    
end
		  
structure Translate : TRANSLATE =
struct
structure F = MipsFrame
structure T = Tree
structure Err = ErrorMsg
datatype level = top
	       | otherLevel of {parent: level, frame: F.frame, unique: unit ref}
datatype exp = Ex of Tree.exp
	     | Nx of Tree.stm
	     | Cx of Temp.label * Temp.label -> Tree.stm
type access = level * F.access
type frag = F.frag
exception LevelError
exception Error
	      
val fragList : (F.frag) list ref = ref []
fun seq ([s1]) = s1
  | seq ([s1,s2]) = Tree.SEQ(s1, s2)
  | seq (s1::seqlist)= Tree.SEQ(s1, seq(seqlist))
  | seq [] = Tree.EXP(Tree.CONST 0 )

fun printLevel (top) = print("top\n")
  | printLevel (otherLevel{parent= parent, frame=f1, unique = u1}) = (print(Symbol.name( #label f1)); print("parent = ");  printLevel(parent))
			   
fun unEx(Ex e) = e
  | unEx (Cx genstm) =
    let val r = Temp.newtemp()
	val t = Temp.newlabel() and f = Temp.newlabel()
    in T.ESEQ(seq[T.MOVE(T.TEMP r, T.CONST 1),
		  genstm(t,f),
		  T.LABEL f,
		  T.MOVE(T.TEMP r, T.CONST 0),
		  T.LABEL t],
	      T.TEMP r)
    end
  | unEx (Nx s) = T.ESEQ(s, T.CONST 0)

fun unCx(Cx c) = c
  | unCx (Ex (T.CONST 0)) = (fn(t,f) => T.JUMP(T.NAME(f), [f]))
  | unCx (Ex (T.CONST 1)) = (fn(t,f) => T.JUMP(T.NAME(t), [t]))
  | unCx (Ex e) = (fn(t,f) => T.CJUMP(T.EQ, T.CONST 1, e, t, f))
  | unCx (Nx _) = (Err.error 0 "Error: Cannot not Uncx an Nx"; fn(t, f) =>  T.JUMP(T.NAME(f), [f]))

fun unNx(Nx x) = x
  | unNx (Ex e) = T.EXP(e)
  | unNx (c) = unNx(Ex(unEx(c)))

fun printTreeHelper (exp) = Printtree.printtree(TextIO.stdOut, unNx(exp))
fun reset () = fragList := []
		    
fun transSeq [] = Ex(Tree.CONST 0)
  | transSeq [exp'] = exp'
  | transSeq expList =
    let
	val backwards = rev(expList)
	val headOfList = hd (backwards)
	val others = rev(tl(backwards))
			
    in
	Ex(Tree.ESEQ(seq(map unNx others), unEx(headOfList)))

    end

fun getResult () = !fragList
		   
fun getFrame (top,_) = (Err.error 0 "Top cannot be first argument of getFrame"; raise LevelError)
   | getFrame (_, top) = (Err.error 0 "Top cannot be second argument of getFrame"; raise LevelError)
   | getFrame (decLevel as otherLevel{parent=_, frame=f1, unique=decUnique},
	       useLevel as otherLevel{parent=parentuse, frame=f2, unique=useUnique}) =
	 if (decUnique=useUnique)
	 then Ex(Tree.TEMP(F.FP))
	 else Ex(Tree.MEM(unEx(getFrame(decLevel, parentuse))))
fun printonefrag (F.PROC{body = body', frame = frame'}) =
    let
    in
	print("function: ");
	print(Symbol.name(#label frame'));
	print("\n");
	printTreeHelper(Nx(body'))
    end
	
  | printonefrag (F.STRING(label', string')) =
    let
    in
	print("string: ");
	print(Symbol.name(label'));
	print("\n");
	print(string');
	print("\n")
	
    end
	

fun simpleVar ((top,_), _) = (Err.error 0 "Function cannot be declared at top level"; raise LevelError)
   | simpleVar ((_, _), top) = (Err.error 0 "Function cannot be declared at top level"; raise LevelError)
   | simpleVar ((decLevel, acc),uselevel) = Ex(F.exp(acc,unEx(getFrame(decLevel, uselevel))))

fun transAssign (expLeft, expRight) = Nx(Tree.MOVE(unEx(expLeft), unEx(expRight)))
						 
fun transRelop (ty, relop, exLeft, exRight) =
    case(ty, relop) of
	(Types.STRING, Tree.EQ) => Ex(F.externalCall("checkStringsEQ", [unEx(exLeft), unEx(exRight)]))
      | (Types.STRING, Tree.LE) => Ex(F.externalCall("checkStringsLE", [unEx(exLeft), unEx(exRight)]))
      | (Types.STRING, Tree.LT) => Ex(F.externalCall("checkStringsLT", [unEx(exLeft), unEx(exRight)]))
      | (Types.STRING, Tree.GE) => Ex(F.externalCall("checkStringsGE", [unEx(exLeft), unEx(exRight)]))
      | (Types.STRING, Tree.GT) => Ex(F.externalCall("checkStringsGT", [unEx(exLeft), unEx(exRight)]))
      | (Types.STRING, Tree.NE) => Ex(F.externalCall("checkStringsNEQ", [unEx(exLeft), unEx(exRight)]))
      | (_,_) => Cx(fn(t,f) => Tree.CJUMP(relop, unEx(exLeft), unEx(exRight), t,f))

fun transLet(letExpList, body) =
    let
	fun ExpListtoStm(a::l) = unNx(a) :: ExpListtoStm(l)
	  | ExpListtoStm ([]) = []
    in
	Ex(Tree.ESEQ(seq(ExpListtoStm(letExpList)), unEx(body)))
    end

fun translateArithmetic (operator, e1, e2) =
    let
	val e1' = unEx(e1);
	val e2' = unEx(e2);
    in
	Ex(T.BINOP(operator, e1', e2'))
    end

(*fun translateConditional (cond, e1, e2) =
    let
	val e1' = unEx(e1)
	val e2' = unEx(e2)
	val cond' = unEx(cond)
	val t1 = Temp.newlabl()
	val f1 = Temp.newlabel()
    in
	Cx(Tree.CJUMP(cond', e1', e2', t1, f1))
    end*)
fun transNilExp() = Ex(Tree.CONST 0)
			
fun translateIf(c, e1, e2) =
    let val c' = unCx(c);
	val e1' = unEx(e1);
	val e2' = unEx(e2);
	val r = Temp.newtemp();
	val t = Temp.newlabel();
	val f = Temp.newlabel();
	val le = Temp.newlabel();
	
    in
	Ex(Tree.ESEQ(seq[c'(t,f),
			Tree.LABEL(t),
			(*Tree.MOVE(Tree.TEMP(r), e2'),*)
			Tree.MOVE(Tree.TEMP(r), e1'),
			Tree.JUMP(Tree.NAME(le), [le]),
			Tree.LABEL(f),
			(*Tree.MOVE(Tree.TEMP(r), e3'),*)
			Tree.MOVE(Tree.TEMP(r), e2'),
			Tree.LABEL(le)],
		  Tree.TEMP(r)))
    end

fun translateIfNoElse(c, e1) =
    let val c' = unCx(c);
	val e1' = unEx(e1);
	
	val r = Temp.newtemp();
	val t = Temp.newlabel();
	
	val le = Temp.newlabel();
	
    in
	Ex(Tree.ESEQ(seq[c'(t,le),
			Tree.LABEL(t),
			(*Tree.MOVE(Tree.TEMP(r), e2'),*)
			Tree.MOVE(Tree.TEMP(r), e1'),
			Tree.JUMP(Tree.NAME(le), [le]),

			(*Tree.MOVE(Tree.TEMP(r), e3'),*)

			Tree.LABEL(le)],
		  Tree.TEMP(r)))
    end


	
fun transMkRec(initlist) =
    let val initlist' = map unEx initlist;
	val len = List.length(initlist')
	val r = Temp.newtemp();
	fun st([], _) = []
	  | st (e::l, idx) = Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST(idx), T.TEMP(r))), e)::st(l, idx+4)
    in
	Ex(Tree.ESEQ
	       (seq(
		    Tree.MOVE(
			  Tree.TEMP(r),
			  Tree.CALL(
			      Tree.NAME(Symbol.symbol "malloc"), [Tree.CONST(4 * len)]))
		     ::st(initlist', 0)),
	  Tree.TEMP(r)))
    end
	
fun transMkArray(arraySize, arrayInitExp) =
    
    let val size = Temp.newtemp()
	val arr = MipsFrame.externalCall("initArray", [Tree.BINOP(Tree.PLUS,Tree.TEMP size,Tree.CONST 1), unEx(arrayInitExp)])
	val temp = Temp.newtemp()
    in
	Ex(Tree.ESEQ( seq([Tree.MOVE(Tree.TEMP size, unEx(arraySize)),
		       Tree.MOVE(Tree.TEMP temp, Tree.BINOP(Tree.PLUS, arr, Tree.CONST 4)),
		       Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.MINUS,Tree.TEMP temp, Tree.CONST 4)), Tree.TEMP size)]),
		      Tree.TEMP temp))
    end 

fun transWhile(condition, body, break) = (*use the form jump, body, cond*)
    let
	val conditionLabel = Temp.newlabel()
	val bodyLabel = Temp.newlabel()
	val condition' = unCx(condition)
	val body' = unNx(body)
    in
	Nx(seq([
		Tree.LABEL(conditionLabel),
		condition'(bodyLabel, break),
		Tree.LABEL(bodyLabel),
		body',
		Tree.JUMP(Tree.NAME(conditionLabel) ,[conditionLabel]),
		Tree.LABEL(break)]))
    end


fun transFor(var, low, high, body, break) =
    let
	val low' = unEx(low)
	val high' = unEx(high)
	val var' = unEx(var)
	val body' = unNx(body)
	val bodyLabel = Temp.newlabel()
	val incrementLabel = Temp.newlabel()
    in
	Nx(seq[
		Tree.MOVE(var', low'),
		Tree.CJUMP(Tree.LE, var', high', bodyLabel, break),
		Tree.LABEL(incrementLabel),
		Tree.MOVE(var', Tree.BINOP(Tree.PLUS, var', Tree.CONST(1))),
		Tree.LABEL(bodyLabel),
		body',
		Tree.CJUMP(Tree.LT, var', high', incrementLabel, break),
		Tree.LABEL(break)
		])
    end


fun transBreak(breakLabel) =Nx(Tree.JUMP(Tree.NAME(breakLabel), [breakLabel]))
			      
    
val outermost = top
(*pg 141 for newLevel function creates new nesting level for each function; calls Frame.newFrame to make a new frame*)	    
fun newLevel({parent, name, formals}) =
    let
	val newformals = true::formals
    in
	otherLevel({
	    parent= parent,
	    frame=F.newFrame{name = name, formals = newformals},
	    unique = ref ()})
    end
(*pg 142 for allocLocal; creates variable at argument level; returns a Translate.access; calls Frame.allocLocal *)

fun frameFromLevel(level) =
    case level of top => (Err.error 0 "Error: Cannot process a function declaration at the top level"; raise Error)
		| otherLevel{parent = parentArg, frame = frameArg, unique=uniqueArg} => frameArg
								    
fun allocLocal(levelArg, escapeArg)=
    let
	fun boolprint(bool) = if bool then print("1") else print ("0")
    in
	
    case levelArg of
	top => (Err.error 0 "Error: Cannot process a local variable declaration at the top level"; raise Error)
      | otherLevel{parent = parentArg, frame = frameArg, unique=uniqueArg} =>

	    

	 (otherLevel{parent = parentArg, frame = frameArg, unique=uniqueArg}, MipsFrame.allocLocal(frameArg)(escapeArg))
	   
    end
	


(*pg 143. formals converts offsets given in levels to access values; access offset values by called Frame.formals(frame)*)
fun formals top = []
  | formals (l as otherLevel({parent = parentArg, frame = frameArg, unique = uniqueArg})) =
    let
	val accessLevelList = [];
	val frameFormals = F.formals(frameArg);
	fun getAccessLevel(frameAccess, accesslist) = (l, frameAccess) :: accesslist (*adds access (level * Frame.access) to accesslist*)
    in
	foldl getAccessLevel accessLevelList frameFormals
    end
					    
fun transFieldVar(r, index) =
    let 
	val r' = unEx(r)
    in
	
	Ex(Tree.MEM(Tree.BINOP(Tree.PLUS, r', Tree.BINOP(Tree.MUL, Tree.CONST 4, Tree.CONST index))))
    end
fun transSubscriptVar (a,  i) =
    (*TODO: MAKE SURE TO COME BACK TO THIS*)
    let
	val a' = unEx(a)
	val i' = unEx(i)
	val t = Temp.newtemp();
	val trueLabel = Temp.newlabel();
	val falseLabel = Temp.newlabel();
	val join = Temp.newlabel();
    in
	
	Ex(Tree.ESEQ(seq([Tree.CJUMP(T.LT, i', Tree.MEM(Tree.BINOP(Tree.MINUS, a', Tree.CONST 4)), trueLabel, falseLabel),
			  Tree.LABEL(trueLabel),
			  (*TODO if i' is GE 0*)
			  Tree.MOVE(Tree.TEMP t, Tree.MEM(Tree.BINOP(Tree.PLUS, a', Tree.BINOP(Tree.MUL, i', Tree.CONST 4)))),
			  Tree.JUMP(Tree.NAME(join), [join]),
			  Tree.LABEL(falseLabel),
			  Tree.EXP(MipsFrame.externalCall("exit", [(Tree.CONST(0))])),
			  Tree.LABEL(join)]),
		     Tree.TEMP t))
    end

					      
fun transSTRING (str : string) =
    let fun isString (F.PROC({body= _ , frame = _}): F.frag) = false
	  | isString (F.STRING (l, s): F.frag) = str = s
	val str' = List.find(isString) (!fragList)
    in case str' of
	   SOME (F.STRING(l, s)) => Ex(T.NAME l)
	 | NONE => let
	             val lab = Temp.newlabel ()
		   in (fragList := F.STRING (lab, str) :: (!fragList);
		       Ex(Tree.NAME lab)) 
		   end
    end

    
fun SLtoPass (callerLevel as otherLevel{parent=p1, frame=f1, unique=callUnique}, calleeLevel as otherLevel{parent=p2, frame=f2, unique=calleeUnique} ) =
    let
	
	fun levelsEqual(otherLevel {parent = _, frame=_, unique = u1}, otherLevel{parent=_, frame=_, unique= u2}) = (u1=u2)
	  | levelsEqual  (top, top) = true
	  | levelsEqual (_,_) = false
    in
	if levelsEqual(p1, calleeLevel) then Ex(Tree.MEM(Tree.TEMP(F.FP)))
	
	else getFrame(callerLevel, calleeLevel)
											 
    end
  | SLtoPass (callerLevel as otherLevel{parent=p1, frame=f1, unique=callUnique}, top) = (print("oh my gosh f1 is trying to call a top level function\n"); raise Fail("failure"))
  | SLtoPass (top, calleeLevel as otherLevel{parent=p2, frame=f2, unique=calleeUnique})  = (print("oh my gosh top is trying to call a"); print(Symbol.name(#label f2)); print(" level function\n"); raise Fail("failure"))
	
fun callExp (callerLevel, top, label, explist) =
    let
	val unExdExplist = map unEx explist
    in
	Ex(Tree.CALL(Tree.NAME(label), unExdExplist))
    end
	
   | callExp (callerLevel, level, label, explist) =
     let
	 
	 val staticLink = unEx(SLtoPass(callerLevel, level))

	 val unExdExplist = map unEx explist
	 
			
     in
	 Ex(Tree.CALL(Tree.NAME(label), staticLink::unExdExplist))
     end

		    
 fun procEntryExit ({level=top, body=body})= (Err.error 0 "Function cannot be declared at top level"; raise LevelError)
   | procEntryExit ({level=otherLevel{parent=parent, frame=frame, unique=uniq}, body=b}) = 
     fragList := ((!fragList) @ [(F.PROC{body=Tree.MOVE(Tree.TEMP(F.RV), unEx(b)), frame=frame})])

(* fun getAddress (access:access, level) = F.exp(#access access, T.TEMP(F.FP))*)

end
