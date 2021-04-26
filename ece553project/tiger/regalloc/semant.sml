 (*NEED TO INCORPORATOAE LEVELS AND BREAK INTO WHILE AND FOR LOOPS and CALL EXP*)
structure Semant =
struct
structure A = Absyn
structure E = Env
structure T = Tree
structure Translate = Translate
structure S = Symbol
structure Err= ErrorMsg

type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table
type expty = {exp:Translate.exp, ty: Types.ty}
(*fun getVal (S.symbol(s,n)):int = n
fun comparator (x:S.symbol, y:S.symbol) = Int.compare(getVal x, getVal y)

structure SymbolSet = RedBlackSetFn (struct type ord_key = S.symbol
					    val compare = comparator
							      end)*)
exception ziperror
exception LookupError	      
exception DeclarationError
exception Nondigit
exception eexception
exception Neg
exception BreakError
exception TypeError	      
fun printSymbolTyPair (s:S.symbol, t:Types.ty) = let in print(Symbol.name s); print(" ") end
fun printSymbolTyPairList symTyList = map printSymbolTyPair symTyList

fun printTypes(Types.NIL) = print("nil ")
		 | printTypes (Types.STRING) = print("string\n")
		 | printTypes (Types.INT) = print("int\n")
		 | printTypes (Types.UNIT) = print("unit ")
		 | printTypes (Types.BOTTOM) = print("bottom ")
		 | printTypes (Types.ARRAY(t,_)) = (print("array of "); printTypes(t))
		 | printTypes (Types.RECORD(g,_)) = (print("record of "); printSymbolTyPairList(g()); print("\n"))
		 | printTypes (_) = print("other ")

					  

fun printEnvEntry(E.VarEntry{access = acc, ty=t})=

    let
    in
	printTypes(t);
	print("\n")
    end

  | printEnvEntry (E.FunEntry{formals=tylist,label=lab, level=lev, result=resty}) =
    let
    in

	map printTypes tylist;
	print(" -> ");
	printTypes(resty);
	print("\n")
    end

fun i2c 0 = #"0"
  | i2c 1 = #"1"
  | i2c 2 = #"2"
  | i2c 3 = #"3"
  | i2c 4 = #"4"
  | i2c 5 = #"5"
  | i2c 6 = #"6"
  | i2c 7 = #"7"
  | i2c 8 = #"8"
  | i2c 9 = #"9"
  | i2c _ = raise Nondigit

fun listDigits 0 = []
  | listDigits x = if (x>0) then (x mod 10)::listDigits(x div 10) else raise Neg

fun i2a n = String.implode (map i2c (rev (listDigits n)))
fun checkInt ({exp=_, ty= Types.INT}, pos) = ()
  | checkInt ({exp=_, ty = _}, pos) =
    let
    in
	Err.error pos "Error: Not an Integer";
	raise TypeError
    end

fun checkString ({exp=_, ty= Types.STRING}, pos) = ()
  | checkString ({exp=_, ty=_}, pos) =
    let
    in
	Err.error pos "Error: Not a String";
	raise TypeError
    end

fun zip ([], []) = []
  | zip (x::xs, y::ys) = (x,y)::zip(xs, ys)
  | zip (_, _) = raise ziperror

fun checkEqTypes(type1, type2, pos, errorMessage) =
    let in
	(*print("\nin check eq types\n");
	printTypes(type1);
	printTypes(type2);*)
	if Types.eq(type1, type2) then ()

	else (Err.error pos errorMessage; raise TypeError)
    end 
val breakdepth : int ref = ref 0
fun checkEqTypesComp(left, right, pos) = if Types.eq(left, right) then ()
					 else (Err.error pos "Error: Incompatible types in comparison";raise TypeError)

fun transExp (venv, tenv, exp, level):expty =
    let	
	fun trexp   (A.VarExp(var)) = transVar (var,level)
	  | trexp   (A.NilExp) = {exp = Translate.transNilExp(), ty= Types.NIL}
	  | trexp   (A.IntExp(integer)) = {exp = Translate.Ex(Tree.CONST integer), ty= Types.INT}(*TODO Check*)
	  | trexp   (A.OpExp{left, oper=A.PlusOp, right, pos}) =
		     (checkInt(trexp left, pos);
		      checkInt(trexp right, pos);
		      {exp=Translate.translateArithmetic(Tree.PLUS, #exp (trexp left), #exp (trexp right)), ty=Types.INT})
	  | trexp   (A.OpExp{left, oper=A.MinusOp, right, pos}) =
		     (checkInt(trexp left, pos);
		      checkInt(trexp right, pos);
		      {exp=Translate.translateArithmetic(Tree.MINUS, #exp (trexp left), #exp (trexp right)), ty=Types.INT})
	  | trexp   (A.OpExp{left, oper=A.TimesOp, right, pos}) =
	    (checkInt(trexp left, pos);
		      checkInt(trexp right, pos);
		      {exp=Translate.translateArithmetic(Tree.MUL, #exp (trexp left), #exp (trexp right)), ty=Types.INT})
	  | trexp   (A.OpExp{left, oper=A.DivideOp, right, pos}) =
		     (checkInt(trexp left, pos);
		      checkInt(trexp right, pos);
		      {exp=Translate.translateArithmetic(Tree.DIV, #exp (trexp left), #exp (trexp right)), ty=Types.INT})
	  | trexp   (A.OpExp{left, oper=A.EqOp, right, pos}) =
		      (checkEqTypesComp(#ty(trexp left), #ty(trexp right), pos);
		      {exp=Translate.transRelop((#ty(trexp left), Tree.EQ, #exp (trexp left), #exp (trexp right))), ty=Types.INT})
	  | trexp   (A.OpExp{left, oper=A.NeqOp, right, pos}) =
	    (checkEqTypesComp(#ty(trexp left), #ty(trexp right), pos);
	     {exp=(Translate.transRelop((#ty(trexp left),Tree.NE, #exp (trexp left), #exp (trexp right)))), ty=Types.INT})
	  | trexp   (A.OpExp{left, oper=A.GtOp, right, pos}) =
	    (checkEqTypesComp(#ty(trexp left), #ty(trexp right), pos);
		      {exp=(Translate.transRelop((#ty(trexp left),Tree.GT, #exp (trexp left), #exp (trexp right)))), ty=Types.INT})
	 | trexp   (A.OpExp{left, oper=A.LtOp, right, pos}) =
	   (checkEqTypesComp(#ty(trexp left), #ty(trexp right), pos);
		      {exp=(Translate.transRelop((#ty(trexp left),Tree.LE, #exp (trexp left), #exp (trexp right)))), ty=Types.INT})
	 | trexp   (A.OpExp{left, oper=A.LeOp, right, pos}) =
	    (checkEqTypesComp(#ty(trexp left), #ty(trexp right), pos);
		      {exp=(Translate.transRelop((#ty(trexp left),Tree.LE, #exp (trexp left), #exp (trexp right)))), ty=Types.INT})
	 | trexp   (A.OpExp{left, oper=A.GeOp, right, pos}) =
	   (checkEqTypesComp(#ty(trexp left), #ty(trexp right), pos);
		      {exp=(Translate.transRelop((#ty(trexp left),Tree.GE, #exp (trexp left), #exp (trexp right)))), ty=Types.INT})
	 | trexp   (A.StringExp(stringval, pos)) = {exp = Translate.transSTRING(stringval), ty =Types.STRING}
	 | trexp   (A.LetExp{decs=decs, body=body, pos=pos}) =
	   let
	      
	       
	       val{tenv=tenv', venv=venv', explist=el}= foldl transDecHelper {tenv=tenv, venv=venv, explist=[]} decs
		
	       val bodyexp = transExp(venv', tenv', body, level)
				    
	   in
	       
	       {exp=Translate.transLet(el,#exp bodyexp), ty=(#ty bodyexp) }		       
	   end
	 | trexp   (A.AssignExp{var, exp, pos}) =
	   let
	       val{exp = valexp, ty = valtype} = transVar (var, level)
	       val {exp = expexp, ty= exptype} = trexp exp
	   in
	       checkEqTypes(valtype, exptype, pos, "Error: Type mismatch");
	       {exp = (Translate.transAssign(valexp, expexp)), ty=Types.UNIT}
	   end
	 | trexp   (A.IfExp{test, then', else', pos}) =
	   (*check if test is of int type *)
	   (*need to check matching types for then' and else'*)
	   (*ty = return type of then'*)
	   (case else' of NONE =>
			  let
			  in
			      checkEqTypes(#ty(trexp then'), Types.UNIT, pos, "Error: No else, then does not return unit");
			      checkInt((trexp test), pos);

			       {exp =  (Translate.translateIfNoElse(#exp (trexp(test)), #exp(trexp then'))), ty= #ty(trexp then')}

			  end
			| SOME(exp) =>
			  let
			  in
			      checkInt((trexp test), pos);
			      checkEqTypes(#ty(trexp then'), #ty(trexp exp), pos, "Error: Return type of then and else do not match");
			      {exp = (Translate.translateIf(#exp (trexp(test)), #exp(trexp then'), #exp(trexp exp))), ty= #ty(trexp then')}
			  end)

	 | trexp   (A.BreakExp(pos)) =
	   let
	       val break = Temp.newlabel();
	   in
	       
	   (if !breakdepth = 0 (*not in a for or while loop*)
	    then (Err.error pos "Error: Break is not within a loop"; raise BreakError)
	    else ();
	    {exp = (Translate.transBreak(break)), ty = Types.UNIT})
	   end
	       


 (*NEED TO INCORPORATE LEVELS AND BREAK INTO WHILE AND FOR LOOPS and CALL EXP*)
	 | trexp   (A.WhileExp{test, body, pos})=
	   let
	       val breakLabel = Temp.newlabel();
	   in
	       
	       (checkEqTypes(#ty(trexp test), Types.INT, pos, "Error: Type of test does not evaluate to an int"); (*write out checkEqTypes*)
		breakdepth := !breakdepth + 1;

		checkEqTypes(#ty(trexp body), Types.UNIT, pos, "Error: Type of body does not evaluated to an unit");
		breakdepth := !breakdepth -1;
		{exp = (Translate.transWhile(#exp(trexp test), #exp(trexp body), breakLabel)), ty = Types.UNIT})
	    end

	 | trexp   (A.ForExp{var, escape, lo, hi, body, pos}) =
	   let
	       val breakLabel = Temp.newlabel();

	       val {exp=expLo, ty=tyLo} = trexp(lo);
	       val {exp=expHi, ty=tyHi} = trexp(hi);
	       val varAccess:Translate.access = Translate.allocLocal(level, !escape)
	       val venv' = S.enter(venv, var, Env.VarEntry({access=varAccess, ty=Types.INT}))
	       val varLoc = Translate.simpleVar(varAccess, level)					       
	   in
	       checkEqTypes(tyLo, Types.INT, pos, "Error: Type of lower bound for for loop is not an int");
	       checkEqTypes(tyHi, Types.INT, pos, "Error: Type of higher bound for for loop is not an int");
	       let val {exp=expBody, ty=bodyTy} = transExp(venv', tenv, body, level)
	       in
		    checkEqTypes(bodyTy, Types.UNIT, pos, "Error: Type of body does not evaluated to an unit");
		    if (Types.eq(bodyTy, Types.UNIT))
		    then ((!breakdepth) = (!breakdepth) - 1;
			  {exp = (Translate.transFor(varLoc, expLo, expHi, expBody, breakLabel)), ty = Types.UNIT})
		    else (!breakdepth = (!breakdepth) -1;
			  {exp=Translate.transNilExp(), ty=Types.UNIT})
			     
		end		   

	   end





	 | trexp   (ce as A.CallExp{func, args, pos}) =
	(*look up type environment for symbol --> returns something then it will be a env object with a formalist, does not return something then error*)
	   (*check if the arguments have the correct types (type environment funentry formallist(list of types of args))*)
	   let fun checkMatchingArgs (formalType::formalTypeList, arg::argList, pos) = if Types.eq(formalType, #ty(trexp arg))
										       then checkMatchingArgs(formalTypeList, argList, pos)
										       else  let
										       in Err.error pos "Error: formal type does not match argument type"; raise TypeError
										       end
												 
		 | checkMatchingArgs ([], [], pos) = ()
		 | checkMatchingArgs (formalType::formalTypeList, [], pos) =
		   let in
		       Err.error pos "Error: Length of arguments expected and arguments given does not match";
		       raise TypeError
		   end
		       
		 | checkMatchingArgs ([], arg::argList, pos) =
		   let in
		       Err.error pos "Error: Length of arguments expected and arguments given does not match";
		       raise TypeError
		   end		       
	       fun argToExp(a::l) = #exp (trexp a) :: argToExp(l)
		 | argToExp ([]) = []
	       val argExps = argToExp(args);
	       

	   in
	       case S.look(venv, func) of
		   (*look for function in gamma, check if there are n argyments given and if the exp matches with the type*)
		   SOME(E.FunEntry({level= declevel, label, formals=formals, result=resultTy})) =>
		   let
		      (* fun printEnvFunEntry(E.FunEntry({formals= formals, result= resultTy})) = map printTypes formals::result
		       fun printvenv(venv) = map printEnvFunEntry (S.Table.listKeys(venv))*)
	
			
		   in
		      ( if List.length(formals) = List.length(args)
		       then   checkMatchingArgs(formals, args, pos)
			else (Err.error pos "Error: Length of formals and argument list are not equal"; raise DeclarationError);
			
		
			{exp=(Translate.callExp(level, declevel, label, argExps)), ty = resultTy})
		   end

		 | SOME(_) => ((Err.error pos "This symbol is not a function"); raise DeclarationError)
		 | NONE => ((Err.error pos "This function does not exist"); raise DeclarationError)
	   end
	 | trexp   (A.ArrayExp{typ, size, init, pos})=
	    let fun getType(SOME(ty)) = ty
		  | getType(NONE) = Types.BOTTOM
		fun typeExists ty = case ty of Types.NAME(name, typeRef) => typeExists(getType(S.look(tenv,name)))
					     | ty => ty
	    in
		case S.look(tenv, typ) of (*Check if name of array is a valid name in the type environment*)
		    SOME(x) =>  (case typeExists x of (Types.ARRAY(ty,unique)) => (*Find type of array in type environment*)
						      (checkInt(trexp size, pos); (*If the type of the array exists in the type environment, check that the sizeof the array is an int and that the type of the initializing expression is the same type of the array*)
						       (*print("init type   ");
						       printTypes(#ty(trexp init));
						       print("\narray type   ");
						       printTypes(ty);*)
						       checkEqTypes(#ty(trexp init), ty, pos, "Error: Type of the initializing expression for the array does not match the type of the array");
						       {exp=(Translate.transMkArray(#exp(trexp size), #exp(trexp init))), ty= x})
						   |  _  => (Err.error pos "Array is not of ARRAY type when created"; raise DeclarationError))
		  | NONE => (Err.error pos "Error: Type of the array does not exist"; raise DeclarationError)
		(*{exp = (), ty = transTy(tenv, A.ArrayTy(init, pos))}*)
	    end
	 | trexp  (A.SeqExp(expPosList)) =
	   let
	       fun recurse(exp, pos) =  #ty (trexp exp)
	      
	       val x:Types.ty list = map recurse expPosList;
	       val reversedx: Types.ty list = rev x;
	       fun t(x::xs)  = x
		 | t ([]) = Types.UNIT

	       fun recurse2(exp,pos) = #exp (trexp exp)
					    
	       val expList = map recurse2 expPosList
							   
	   in
	       {exp =(Translate.transSeq(expList)), ty= t(reversedx)}
	   end

	 | trexp  (A.RecordExp{fields=fieldsList, pos=pos,typ= typ})  =
	   (*Search for type of record in type environment*)
	   (case S.look(tenv,typ) of SOME x =>
				     (*check that types in record match types in formallist*)
				     (case x of Types.RECORD(reclistfun, _) =>
						let
						    val reclist = reclistfun ();
						    val recListNames = map #1 reclist;
						    val recTypes: Types.ty list = map #2 reclist;

						    val fieldTypes0: Absyn.exp list = map  #2 fieldsList;
						    val fieldTypes1:  {exp:Translate.exp, ty:Types.ty} list = map trexp fieldTypes0;
						    val fieldTypes: Types.ty list = map #ty fieldTypes1;
						    val fieldNames = map #1 fieldsList
						    val fieldValues = map #exp fieldTypes1
						    fun getName s = S.name s
						    fun checkTypes ([], []) = true
						      | checkTypes (recty::recList, fieldty::fieldsList) = Types.eq(recty, fieldty) andalso checkTypes(recList, fieldsList)
						    fun checkNames([], []) = true
						      | checkNames (recname::recList, fieldname::fieldsList) = recname=fieldname andalso checkNames(recList, fieldsList)

						in
						    (*map printEnvEntry (S.listItems(venv));*)
						    if checkTypes(recTypes, fieldTypes) andalso checkNames(recListNames, fieldNames)
						    then  {exp=(Translate.transMkRec(fieldValues)), ty= x}
						    else (Err.error pos "Fields list does not match the record's formal list"; raise DeclarationError)
						end
					     |  _  => (Err.error pos "Error: Not a record type"; raise DeclarationError))
				   | NONE  => (Err.error pos "Error: Record type does not exist"; raise DeclarationError))

	and transDecHelper (dec, {tenv=tenv, venv=venv, explist=el}) =let
	in

	    transDec (venv, tenv, dec, level, el)
	end

and transDec (venv, tenv, A.VarDec{name=name, escape=esc, typ=NONE, init=init, pos=pos}, level, el) =
    let val {exp=expInit,ty= tyInit} = transExp(venv, tenv, init, level)
	fun checkNotRecord (Types.RECORD(_,_), Types.RECORD(_,_)) = false
	  | checkNotRecord (_,_) = true
	fun dummy () = []
			   
        val varAccess = Translate.allocLocal(level, !esc)

	val varAddress = Translate.simpleVar(varAccess, level)
    in
	if Types.eq(tyInit, Types.NIL) andalso checkNotRecord(tyInit, Types.RECORD(dummy, ref ()))
	then ((Err.error pos "Error: Cannot initialize variable to be nil if not contrained in record type");raise eexception)
	else
	    {tenv = tenv,
	     venv = S.enter(venv, name, E.VarEntry{access=varAccess, ty=tyInit}),
	     explist = el@[Translate.transAssign(varAddress, expInit)]
	    }
    end
	
  | transDec (venv, tenv, A.VarDec{name=name,escape=esc,typ=SOME(typpos), init=init, pos=pos}, level, el) =
    let val {exp=expInit,ty= tyInit} = transExp(venv, tenv, init, level);
	fun fst(a,b) = a;
	val typ = fst(typpos);
	val vartype:Types.ty = valOf(S.look(tenv, typ))

        val varAccess: Translate.access = Translate.allocLocal(level, !esc)
	
	val varAddress = Translate.simpleVar(varAccess, level)
	fun checkRecord(Types.RECORD(_,_))=true
	  | checkRecord _  = false 
    in
(*	print("vartype: ");	
	printTypes(vartype);
	print("tyInit: ");
	printTypes(tyInit);*)
	(* Types.eq(RECORD,NIL) is true but Types.eq(NIL, RECORD) is false!?!!??!
	Anyway, Types.eq(x.Types.NIL) is true if x is NIL or RECORD
	        Types.eq(Types.NIL, x) is only true if x is NIL
	 *)
	if Types.eq(vartype, tyInit)
	then  ({tenv=tenv, venv= S.enter(venv, name, E.VarEntry{access=varAccess, ty=tyInit}), explist=el@[Translate.transAssign(varAddress, expInit)]})
	else (if (Types.eq(Types.NIL, tyInit) andalso checkRecord(vartype))
	      then  (print("Record Initialized with NIL\n"); {tenv=tenv, venv= S.enter(venv, name, E.VarEntry{access=varAccess, ty=vartype}), explist=el})
	      else
		  (Err.error pos "Error: Different Variable Types between initial and constraint"; raise DeclarationError;{tenv= tenv, venv=venv, explist=el}))
    end
  | transDec (venv, tenv, A.TypeDec([]), level, explist) = {tenv=tenv, venv=venv, explist=explist}
(*  | transDec (venv, tenv, A.TypeDec([{name=name, ty=ty, pos=pos}]))  =
    {tenv = S.enter(tenv, name, transTy(tenv, ty)), venv=venv}*)
  | transDec (venv, tenv, A.TypeDec(declist), level, explist) =
    (*only call this if name is in listnames*)
    let fun NameToDec(name, pos) =
	    let fun decsWithName name = List.filter (fn {name=n, ty=ty, pos=pos} => (n = name)) declist;
		val decs = decsWithName name
		fun printDec ({name=n1, ty= ty1, pos=pos1}) = print (Symbol.name n1);
	    in
		(*map printDec decs;*)
		case (length decs) of
		    1 => List.nth(decs,0)
		  | _ => raise DeclarationError
	    end
	fun contains ([], y: S.symbol) = false
	  | contains (x::xs, y) = (x=y) orelse contains(xs,y)
	val listnames = map #name declist
	fun decCompare ({name=n1, ty=t1, pos=p1}, {name=n2, ty=t2, pos=p2}) = (n1 =n2) andalso (t1 =  t2) andalso (p1 =p2)
	(*f adds a declaration to tenv by doing a DFS
	type a = b (decname=a, namesym=b) 
	type b = c (decname =b, namesym=c)
	 *)
	fun notNone(NONE) = false
	  | notNone _ = true						  
	fun inTenv(tySym) = notNone(S.look(tenv, tySym))				  
	fun dfs(dec as {name=decname, ty=decty, pos=decpos}, visited:S.symbol list) =
	    case decty of A.NameTy(namesym, namepos) =>
			  let in (*print("Name Type in DFS\n");*)
			      
			      if (namesym=decname orelse (contains(visited, decname)))
			      then let in (Err.error decpos "Cyclic typedecs detected\n"); raise DeclarationError end
			      else
				  if contains(listnames, namesym) then
				      dfs(NameToDec(namesym, namepos), decname :: visited)
				  else if inTenv(namesym)
				  then false
				  else (Err.error decpos "Unkown Name type\n"; raise DeclarationError) 
			  end
			| A.ArrayTy(namesym, namepos) =>
			  let in 
			      if (namesym=decname orelse (contains(visited, decname)))
			      then let in (Err.error decpos "Cyclic typedecs detected\n");raise DeclarationError end
                              else
				  if contains(listnames, namesym) then
                                      dfs(NameToDec(namesym, namepos), decname :: visited)
				  else if inTenv(namesym)
				  then false
				  else (Err.error decpos "Unkown Name type\n"; raise DeclarationError) 

  			  end
			| A.RecordTy(fieldlist) =>
			  let val fieldTySyms = map #typ fieldlist
			      fun isLegal(tySym) = if (contains(listnames, tySym) orelse inTenv(tySym))
							 then ()
							 else (Err.error decpos "Illegal Type declaration"; raise DeclarationError)	
			  in map isLegal fieldTySyms;
			      false
			  end
			      

	val uniqsForTypes = foldl (fn (name, table) => S.enter(table, name, ref () )) S.empty listnames
	fun lookupTy(tySym, tyPos) = if contains(listnames, tySym)
			      then let val nextDec = NameToDec(tySym, tyPos)
				   in
				       transTy2(nextDec, tenv)
				   end
			      else valOf(S.look(tenv, tySym))
	and transTy2({name=decname, ty=A.NameTy(tySym, tyPos), pos=decpos}, tenv): Types.ty =
	    lookupTy(tySym, tyPos)
	  | transTy2({name=decname, ty=A.ArrayTy(tySym, tyPos), pos=decpos}, tenv) =
	    let val inner = lookupTy(tySym, tyPos)
	    in
		Types.ARRAY(inner, valOf(S.look(uniqsForTypes, decname)))
	    end
	  | transTy2({name=decname, ty=A.RecordTy(fieldlist), pos=decpos}, tenv) = 
	    let val fieldNames = map #name fieldlist
		val fieldTySyms = map (fn({name, escape, typ, pos}) => (typ, pos)) fieldlist
		val uniq =  S.look(uniqsForTypes, decname)
		fun recbody () =  zip (fieldNames, map lookupTy fieldTySyms)
	    in
		Types.RECORD(recbody, valOf(uniq))
	    end
	fun dfsStart x = dfs (x, [])			     
			     
	val numCautions = List.length(List.filter dfsStart declist)
				     
	
		(*fun contains' (item, listOfNames) = if contains(listOfNames,item)
				      then listOfNames 
				      else item::listOfNames
		
		val uniqueNames =foldl contains' [] names*)

	fun anyDuplicates([]) = false
	  | anyDuplicates(name::names) = if contains(names, name)
					 then true
					 else
					     anyDuplicates(names)
    in
	(*checkCycles();*)
	if anyDuplicates(listnames)
	then (Err.error 0 ("Error: duplicate names of types\n") ; raise DeclarationError)
	else if numCautions>0 then (Err.error 0 ("some other error"); raise DeclarationError)
	else {tenv = foldl (fn(dec, tenv') => S.enter(tenv', #name dec, transTy2(dec, tenv'))) tenv declist , venv=venv, explist=explist}
    end

  | transDec (venv, tenv, A.FunctionDec([]), level, explist) = {tenv=tenv, venv=venv, explist=explist}
  | transDec (venv, tenv, A.FunctionDec(dec::decs), level, explist) =
    let
	
	type arg = {name:S.symbol, ty:Types.ty}
	fun getArgs (dec:A.fundec): arg list =
	    let val params: A.field list = #params dec
	        fun transparam ({name=name, escape=escape, typ=typ, pos=pos}:A.field) = case S.look(tenv,typ) of
											    SOME(t) => {name=name, ty=t}
											  | NONE => raise DeclarationError
	    in
		map transparam params
	    end
	fun contains ([], y: S.symbol) = false
	  | contains (x::xs, y) = (x=y) orelse contains(xs,y)
	fun getResult (dec: A.fundec): Types.ty =
	    case (#result dec) of
			      NONE => Types.UNIT (* is this what we do for a procedure?*)
			   |  SOME(resty, respos) => case S.look(tenv, resty) of
							 SOME(x) => x
						       | NONE => raise DeclarationError

	fun getFormals (dec: A.fundec): Types.ty list =
	    let
		val args = getArgs(dec);
	    in
		map #ty args
	    end

	fun getNames (dec: A.fundec): S.symbol list =
	    let
		val args = getArgs(dec);
	    in
		map #name args
	    end

	(*get FunEntry {formals, result} from fundec*)
	fun getFunEntry (dec: A.fundec, level') =
	    let val res = getResult(dec)
		val funLabel = (#name dec)
	    in
		E.FunEntry{formals = getFormals(dec),label=funLabel, level=level', result=res}
	    end
	(*add VarEntry to venv*)
	fun addVarEntry level' ((v:arg,accessArg), venv) =
	    let 
		val venv' = 
		    S.enter(venv, #name v, E.VarEntry{access=accessArg, ty=(#ty v)})			   
	    in
		venv'
	    end

		
	(*Add FunEntry to venv*)
	fun addFunEntry level' (dec: A.fundec, venv):venv =
	    let val entry = getFunEntry(dec, level')
	    in
		S.enter(venv, #name dec, entry)
	    end
	(*Now add Function prototypes to venv*)


	    
	fun addFuncDecstoVENV(level') = foldl (addFunEntry (level')) venv (dec::decs)
	
	val args = getArgs(dec)
	
	
	
	(*Add arguments of a function to venv, provided its prototype has already been added to venv*)
	val names = map #name (dec::decs);
		(*fun contains' (item, listOfNames) = if contains(listOfNames,item)
				      then listOfNames 
				      else item::listOfNames
		
		val uniqueNames =foldl contains' [] names*)
	fun anyDuplicates([]) = false
	  | anyDuplicates(name::names) = if contains(names, name)
					 then true
					 else
					     anyDuplicates(names)
 	fun trdec (declare, {venv=venv, el=el}) =
	    let		
		val formals' = map (fn(field) => !(#escape field)) (#params declare)
		val level' = Translate.newLevel({parent=level, name =(#name declare), formals=formals'});
		val frame' = Translate.frameFromLevel(level')
		val argsagain = getArgs(declare)
		val (SLaccess::argaccesses) = Translate.formals(level')
		val argaccesslist = zip(argsagain, argaccesses)			 
		val venv'' = foldl (addVarEntry(level')) venv argaccesslist
		val bodyExpTy = transExp(venv'', tenv, #body declare, level')
				
		val dummy = Translate.procEntryExit({level=level', body= #exp bodyExpTy}) (*TODO what is this*)
		val resTy = getResult(declare);	
	    in		
		if Types.eq(#ty bodyExpTy, resTy)
		then {venv=venv, el=el@[(#exp bodyExpTy)]}
		else
		    let
		    in
			(Err.error 0 ("Error: Return type of function not specified\n");
			 raise DeclarationError)
		    end
	    end
		(*this is OK but when we need to pass exp above, how will we do it?*)
	val dup = if anyDuplicates(names)
		  then (Err.error 0 "Error: Two functions with the same name\n"; raise DeclarationError)
		  else ();

	val venvwithfuncdecs = addFuncDecstoVENV(level);
	val {venv=venv'', el=el''} = foldl trdec {venv=venvwithfuncdecs, el=[]} (dec::decs);
    in	
	{tenv=tenv, venv=venvwithfuncdecs, explist=explist}
    end


and transVar (A.SimpleVar(id, pos), level) =
    (case Symbol.look(venv, id) of SOME(E.VarEntry{access=access, ty=ty}) =>
				   let (*val _ = print((Symbol.name id)^"\n")
				       val _ = Translate.printaccess(access)*)
				   in 
				       {exp = (Translate.simpleVar(access, level)), ty = ty}
				   end										 
				|  NONE => (Err.error pos ("Error: Undefined simple variable"); raise LookupError))
  | transVar (A.FieldVar(v, id, pos), level) =
    let
	val record = #ty (transVar(v, level))
	val {exp=r, ty=tyr} = transVar(v, level)
			     	
	fun checkRecord(Types.RECORD(_,_),Types.RECORD(_,_)) = true
	  | checkRecord(_, _) = false
	fun dummy () = []
	fun checkField(list, field) = List.exists (fn y => field = y) list
    in
	if checkRecord(record, Types.RECORD(dummy, ref ()))
	then (
	    let
		fun getFList (Types.RECORD(fieldfun, _)) = fieldfun ()
		fun getSymbols ([]) = []
		  | getSymbols ((sym,typ)::list) = sym::getSymbols(list)
		val symbols = getSymbols(getFList(record))
		fun getIndex(sym::symList) = if (sym=id) then 0
					     else 1+getIndex(symList)

		  | getIndex ([]) = (Err.error pos ("Error: Cannot access id in Record"); raise eexception)						  
		fun getFieldType ([]) = {exp = Translate.transNilExp(), ty= Types.BOTTOM}
		  | getFieldType ((sym, typ)::list) = if sym = id then {exp = (Translate.transFieldVar(r, getIndex(symbols))), ty= typ}
						      else getFieldType(list)
	
	    in
		(if checkField(symbols, id)
		 then
		     getFieldType(getFList(record))
			
		 else
		     (Err.error pos ("Undefined field"); raise eexception))
		(*{exp = (), ty = record}*)
	    end)
	else
	    ((*printTypes(record);*) Err.error pos ("Variable is not a record"); raise DeclarationError)
    end
  (*ASSUMING THAT TRANSVAR ON AN ARRAY RETURNS A TREE.MEM*)
  | transVar (A.SubscriptVar(v, exp, pos), level) =
    let
	val {exp = expV, ty = tyV} = transVar(v, level)
	
	val check = transExp(venv, tenv, exp, level)
        val typeret = #ty (check)
	fun checkArray (Types.ARRAY(_,_), Types.ARRAY(_,_)) = true
	  | checkArray (_,_) = false
	fun getTypeofArray (Types.ARRAY(ty, unique)) = ty
	  | getTypeofArray (_) = Types.BOTTOM 
    in
	check;
	if checkArray(Types.ARRAY(Types.INT, ref ()), tyV)
	then
	    {exp = (Translate.transSubscriptVar(expV, #exp(check))), ty = getTypeofArray(tyV)}
	else
	    (Err.error pos ("Error: Variable is not an array"); raise DeclarationError)
    end

and transTy (tenv, A.NameTy(id, pos)) =
    (case Symbol.look(tenv, id)
      of SOME(ty) => ty
       | NONE => (Err.error pos ("not a name type");
		  raise DeclarationError;
			Types.BOTTOM))
  | transTy (tenv, A.ArrayTy(id, pos)) =
    (case Symbol.look(tenv, id) of SOME(ty) => Types.ARRAY(ty, ref ())
				 | NONE => (Err.error pos ("not an array type");
					    raise DeclarationError))
  | transTy (tenv, A.RecordTy(fields)) =
    (let
	val unique = ref (); 
		       fun findTy(typ) = (case Symbol.look(tenv, typ) of SOME(ty) => ty
					| NONE => Types.BOTTOM)
		       fun makeList ([]) = []
			 | makeList ((field:A.field)::fieldList) = (#name field, findTy(#typ field))::makeList(fieldList)
		       fun recordFun () = makeList(fields)
		   in
		       Types.RECORD(recordFun, unique)
		   end)
(*  | transTy (tenv, A.RecordTy(field::fields)) =
    let
	fun checkField(field = ()
    in
	foldl checkField () fields
    end

  | transTy (tenv, declgroup) =
    let
	fun checkTy(name) = if (name in declgroup)
			    then (case rhs of in declgroup is NAME s => checkTy(s)
					      | RECORD flist => fn() => RECORD map ((fn(n,t)=>(n,checkTy(t))) flist))
			    else Symbol.look(tenv, name)
    let val check = trexp exp
	val recurse = transVar(v)
    in
	check; recurse
    end


and transTy (tenv, A.NameTy(id, pos)) =
    (case Symbol.look(tenv, id)
      of SOME(ty) => ty
	     | NONE => (Err.error pos ("not a name type"); Types.INT))
  | transTy (tenv, A.ArrayTy(id, pos)) =
    (case Symbol.look(tenv, id) of SOME(ty) =>ty
				 | NONE => (Err.error pos ("not an array type");  Types.INT))
  (*| transTy (A.RecordTy(venv, tenv, fields)) =
    let fun checkFields = *)
    in
	checkTy
    end
*)
in
    trexp exp
end

fun transProg(thisExp : Absyn.exp) =
    let val label = Temp.namedlabel("tig_main")
	val formals: bool list = []
	val newlevel = Translate.newLevel{parent=Translate.outermost, name=label, formals=formals}
	val () = FindEscape.findEscape thisExp
    in
	Translate.procEntryExit({level=newlevel, body = #exp(transExp (E.base_venv, E.base_tenv, thisExp, newlevel) ) });

	(*	Translate.printTreeHelper(#exp(transExp (E.base_venv, E.base_tenv, thisExp, newlevel)));*)
	Translate.getResult()
    end

end

(*structure Main =
struct

fun compiler file =
    let
	val () = Translate.reset()
	val parseTree = Parse.parse(file)
	(*val () = escapeAnalysis()*) (*DO THIS*) 
    in

	Semant.transProg(parseTree)
    end
	
fun c num = compiler("test"^Semant.i2a(num)^".tig")		       
end
*)
