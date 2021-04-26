signature ENV =
sig
    type access
    type ty = Types.ty
    datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                      | FunEntry of {level: Translate.level,
				     label: Temp.label,
				     formals: ty list, result: ty}
    val base_tenv : ty Symbol.table (* predefined types*)
    val base_venv : enventry Symbol.table (* predefined functions*)
end

structure Env :> ENV =
struct
type access=unit
type ty = Types.ty
datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                  | FunEntry of {level: Translate.level,
				 label: Temp.label,
				 formals: ty list, result: ty}

val base_tenv: ty Symbol.table =
    let val tenv_with_int = Symbol.enter(Symbol.empty, Symbol.symbol("int"), Types.INT)
    in
	Symbol.enter(tenv_with_int, Symbol.symbol("string"), Types.STRING)
    end

val base_venv: enventry Symbol.table =
    let val baseList = [
	    (Symbol.symbol("print"), FunEntry{level = Translate.outermost, label = Temp.namedlabel("tig_print"), formals = [Types.STRING], result=Types.UNIT}),
	    (Symbol.symbol("flush"), FunEntry{level = Translate.outermost, label = Temp.namedlabel("tig_flush"), formals = [], result = Types.UNIT}),
	    (Symbol.symbol("getchar"), FunEntry{level = Translate.outermost, label = Temp.namedlabel("tig_getchar"), formals = [], result = Types.STRING}),
	    (Symbol.symbol("ord"), FunEntry{level = Translate.outermost, label = Temp.namedlabel("tig_ord"), formals=[Types.STRING], result=Types.INT}),
	    (Symbol.symbol("chr"), FunEntry{level = Translate.outermost, label = Temp.namedlabel("tig_chr"), formals=[Types.INT], result= Types.STRING}),
	    (Symbol.symbol("size"), FunEntry{level = Translate.outermost, label = Temp.namedlabel("tig_size"), formals=[Types.STRING], result = Types.INT}),
	    (Symbol.symbol("substring"), FunEntry{level = Translate.outermost, label = Temp.namedlabel("tig_substring"), formals=[Types.STRING, Types.INT, Types.INT], result = Types.STRING}),
	    (Symbol.symbol("concat"), FunEntry{level = Translate.outermost, label = Temp.namedlabel("tig_concat"), formals=[Types.STRING, Types.STRING], result=Types.STRING}),
	    (Symbol.symbol("not"), FunEntry{level = Translate.outermost, label = Temp.namedlabel("tig_not"), formals=[Types.INT], result=Types.INT}),
	    (Symbol.symbol("exit"), FunEntry{level = Translate.outermost, label = Temp.namedlabel("tig_exit"), formals=[Types.INT], result=Types.UNIT})]
	fun adderFunction ((sym,ty), table) = Symbol.enter(table, sym, ty)
    in
	foldl adderFunction Symbol.empty baseList
    end
	
			 
end
    
