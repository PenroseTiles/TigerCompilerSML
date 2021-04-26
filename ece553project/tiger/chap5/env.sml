signature ENV =
sig
    type access
    type ty = Types.ty
    datatype enventry = VarEntry of {ty: ty}
                      | FunEntry of {formals: ty list, result: ty}
    val base_tenv : ty Symbol.table (* predefined types*)
    val base_venv : enventry Symbol.table (* predefined functions*)
end

structure Env :> ENV =
struct
type access=unit
type ty = Types.ty
datatype enventry = VarEntry of {ty: ty}
                      | FunEntry of {formals: ty list, result: ty}

val base_tenv: ty Symbol.table =
    let val tenv_with_int = Symbol.enter(Symbol.empty, Symbol.symbol("int"), Types.INT)
    in
	Symbol.enter(tenv_with_int, Symbol.symbol("string"), Types.STRING)
    end

val base_venv: enventry Symbol.table =
    let val baseList = [
	    (Symbol.symbol("print"), FunEntry{formals = [Types.STRING], result=Types.UNIT}),
	    (Symbol.symbol("flush"), FunEntry{formals = [], result = Types.UNIT}),
	    (Symbol.symbol("getchar"), FunEntry{formals = [], result = Types.STRING}),
	    (Symbol.symbol("ord"), FunEntry{formals=[Types.STRING], result=Types.INT}),
	    (Symbol.symbol("chr"), FunEntry{formals=[Types.INT], result= Types.STRING}),
	    (Symbol.symbol("size"), FunEntry{formals=[Types.STRING], result = Types.INT}),
	    (Symbol.symbol("substring"), FunEntry{formals=[Types.STRING, Types.INT, Types.INT], result = Types.STRING}),
	    (Symbol.symbol("concat"), FunEntry{formals=[Types.STRING, Types.STRING], result=Types.STRING}),
	    (Symbol.symbol("not"), FunEntry{formals=[Types.INT], result=Types.INT}),
	    (Symbol.symbol("exit"), FunEntry{formals=[Types.INT], result=Types.UNIT})]
	fun adderFunction ((sym,ty), table) = Symbol.enter(table, sym, ty)
    in
	foldl adderFunction Symbol.empty baseList
    end
	
			 
end
    
