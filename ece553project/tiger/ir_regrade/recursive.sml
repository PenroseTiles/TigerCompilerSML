fun f (dec as {name=name, ty=ty, pos=p}, tenv) = 1
(*type b=int*)
(*type intlist = {hd:b, tl:intlist}*)
(*     name      subname:t, subname:t*)
fun makeList A.RecordTy(fieldlist) =
    let val uniq = ref ()
	fun g () =
	    let fun mapper (field as {name=subname,escape=e, typ=t, pos=p}) =
		    if t=name then Types.RECORD(g, unique)
		    else
			valOf(S.look(tenv, subname))
	    in
		map mapper fieldlist
	    end
    in
	Types.RECORD(g, unique)
    end
	
