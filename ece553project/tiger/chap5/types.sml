signature Types = 
sig 
	eqtype ty 
end 
structure Types =
struct

  type unique = unit ref

  datatype ty = 
            RECORD of (unit -> (Symbol.symbol * ty) list) * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
	  | NAME of Symbol.symbol * ty option ref
	  | UNIT
	  | BOTTOM
	
  fun eq(NIL,NIL) = true
    | eq (INT,INT) = true
    | eq (STRING,STRING) = true
    | eq (RECORD(_, unique1), RECORD(_, unique2)) = (unique1 = unique2)
    | eq (RECORD(_, unique1), NIL) = true
    | eq (ARRAY(_, unique1), ARRAY(_, unique2)) = (unique1 = unique2)
    | eq (NAME(symbol1, _), NAME(symbol2, _)) = String.compare(Symbol.name symbol1, Symbol.name symbol2) = EQUAL
    | eq (UNIT, UNIT) = true
    | eq (BOTTOM, _) = true
    | eq (_,_) = false 
			      
end
