signature FRAME =
sig type frame
    type access
    datatype frag = PROC of {body: Tree.stm, frame: frame}
	      | STRING of Temp.label * string
    type register
    val FP: Temp.temp
    val RV: Temp.temp
    val wordSize: int
    val RA: Temp.temp
    val SP: Temp.temp
    val A0: Temp.temp
    val A1: Temp.temp
    val A2: Temp.temp
    val A3: Temp.temp
    val R0: Temp.temp
    val specialregs: Temp.temp list
    val argregs: Temp.temp list
    val calleesaves: Temp.temp list
    val callersaves: Temp.temp list
    val colorableRegisters: register list
    val newFrame: {name: Temp.label, formals: bool list} -> frame
    val name : frame -> Temp.label
    val formals: frame -> access list
    val allocLocal: frame -> bool -> access
    (*val funcEntryExit1: frame*Tree.stm -> Tree.stm*)
    val exp: access * Tree.exp -> Tree.exp
    val externalCall: string * Tree.exp list -> Tree.exp
    val makestring: Temp.temp -> string
    val procEntryExit2: frame * Assem.instr list -> Assem.instr list
    val tempMap: string Temp.Table.table
    val tempToRegisterMap: register Temp.Table.table
    val allocationToString: (register Temp.Table.table)->(Temp.temp) -> string
    val procEntryExit1: frame * Tree.exp * Tree.stm -> Tree.stm
    val procEntryExit3: frame * Assem.instr list * int -> {prolog: string, body: Assem.instr list, epilog: string}
    val printaccess: access ->unit
    val addFragsToSet: frag list -> frag list
end
