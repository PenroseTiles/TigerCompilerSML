structure FindEscape : sig val findEscape: Absyn.exp -> unit
end  = 
struct

structure A= Absyn
structure S= Symbol
		 
type depth = int
type escEnv = (depth * bool ref) Symbol.table

fun traverseVar(env:escEnv, d:depth, s:Absyn.var): unit =
    let
	fun trvar(A.SimpleVar(symbol,pos)) =
	    (case S.look(env, symbol) of
		 SOME(symDepth, escape') => if symDepth<d
				      then escape' := true
				      else ()
	       | _ =>())
	  | trvar (A.FieldVar(var, symbol, pos)) = trvar var
	  | trvar (A.SubscriptVar(var, exp, pos)) =
	    let
	    in
		trvar var;
		traverseExp(env, d, exp)
	    end
    in
	trvar s
    end
	
and traverseExp(env:escEnv, d:depth, s:Absyn.exp) : unit =
    let
	fun trexp(A.VarExp(var)) = traverseVar(env, d, var)
	  | trexp (A.NilExp) = ()
	  | trexp (A.IntExp(int)) = ()
	  | trexp (A.StringExp(string)) = ()
	  | trexp (A.CallExp{func, args, pos}) = app trexp args (*apply trexp to all args*)
	  | trexp (A.OpExp{left, oper, right, pos}) =
	    let
	    in
		trexp left;
		trexp right
	    end
	  | trexp (A.RecordExp{fields, typ, pos}) =
	   let
		fun applyToExpPosList((sym, exp, pos)) = trexp exp
	    in
		app applyToExpPosList fields
	    end
	  | trexp (A.SeqExp(expPosList)) =
	    let
		fun applyToExpPosList((exp, pos)) = trexp exp
	    in
		app applyToExpPosList expPosList
	    end
	  | trexp (A.AssignExp{var=var, exp=exp, pos=pos}) =
	    let
	    in
		traverseVar(env, d, var);
		trexp exp
	    end
	  | trexp (A.IfExp{test, then', else', pos}) =
	    let
	    in
		trexp test;
		trexp then';
		case else' of SOME(exp) => trexp exp
			    | NONE => ()
	    end
	  | trexp (A.WhileExp{test, body, pos}) =
	    let
	    in
		trexp test;
		trexp body
	    end
	  | trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
	    let
		val env' = S.enter(env, var, (d, escape))
	    in
		traverseExp(env', d, lo);
		traverseExp(env', d, hi);
		traverseExp(env', d, body)
	    end
	  | trexp (A.BreakExp(pos)) = ()
	  | trexp (A.LetExp{decs, body, pos}) =
	    let
		val env' = traverseDecs(env, d, decs);
	    in
		traverseExp(env', d, body)
	    end
	  | trexp (A.ArrayExp{typ, size, init, pos}) =
	    let
	    in
		trexp size;
		trexp init
	    end
    in
	trexp s
    end
	
and traverseDecs(env, d, s: Absyn.dec list) : escEnv =
    let
	fun trdec(A.FunctionDec(fundecs), env) =
	    let
		fun addParam({name = name', escape= escape', typ= typ', pos= pos'}, env)=
		    S.enter(env, name', (d+1, escape'))
		fun addFunDecstoEnv({name = name', params=params', result = result', body=body', pos= pos'}) =
		    let
			val env' = foldl addParam env params'
		    in
			traverseExp(env', d+1, body')
		    end
			
	    in
		app addFunDecstoEnv fundecs;
		env
	    end
	  | trdec (A.VarDec{name, escape, typ, init, pos}, env) =
	    let
		val env' = S.enter(env, name, (d, escape))
	    in
		traverseExp(env', d, init);
		env'
	    end
	  | trdec (A.TypeDec(typedeclist), env) = env
	and applyToDecs (dec, env') = trdec(dec, env')
    in
	foldl applyToDecs env s
    end
	
fun findEscape(prog:Absyn.exp):unit = traverseExp(S.empty, 0, prog)
end
    
