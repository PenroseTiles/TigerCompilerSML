signature TRANSLATE =
sig
    type level
    type access

    val outermost: level (*level within which the a program is nested*)
    val newLevel: {parent:level, name:Temp.label, formals: bool list} -> level
									      
    val formals: level -> access list
    val allocLocal: level -> bool -> access
end

structure Translate : TRANSLATE =
struct
 exception LevelError
 structure Frame =MipsFrame
 structure Err=ErrorMsg
 datatype level = top
		| otherLevel of {parent: level, frame: Frame.frame, unique: unit ref}
 val outermost = top
 type access = level * Frame.access
 (*pg 141 for newLevel function creates new nesting level for each function; calls Frame.newFrame to make a new frame*)	    
 fun newLevel({parent, name, formals}) =
     let
	 val newformals = true::formals
     in
	 otherLevel({
		       parent= parent,
		       frame=Frame.newFrame{name = name, formals = newformals},
		       unique = ref ()})
     end
 (*pg 142 for allocLocal; creates variable at argument level; returns a Translate.access; calls Frame.allocLocal *)
 fun allocLocal(levelArg)=
     let fun helper escapeArg = 
	     case levelArg of
		 top => (Err.error 0 "Error: Cannot process a local variable declaration at the top level"; raise LevelError)
	       | otherLevel({parent = parentArg, frame = frameArg, unique=uniqueArg}) =>
		 (otherLevel({parent = parentArg, frame = frameArg, unique=uniqueArg}), Frame.allocLocal(frameArg)( escapeArg))
     in
	 helper
     end	 

 (*pg 143. formals converts offsets given in levels to access values; access offset values by called Frame.formals(frame)*)
 fun formals top = []
   | formals (l as otherLevel({parent = parentArg, frame = frameArg, unique = uniqueArg})) =
     let
	 val accessLevelList = [];
	 val frameFormals = Frame.formals(frameArg);
	 fun getAccessLevel(frameAccess, accesslist) = (l, frameAccess) :: accesslist (*adds access (level * Frame.access) to accesslist*)
     in
	 foldl getAccessLevel accessLevelList frameFormals
     end
	 
 fun getFrame (top,_) = (Err.error 0 "Function cannot be declared at top level"; raise LevelError)
   | getFrame (_, top) = (Err.error 0 "Function cannot be declared at top level"; raise LevelError)
   | getFrame (decLevel as {parent=_, frame=_, unique=decUnique},
	       useLevel as {parent=useParent, frame=_, unique=useUnique}) =
     if (decUnique=useUnique)
     then Tree.TEMP(F.FP)
     else Tree.MEM(getFrame(decLevel, useParent))

 fun simpleVar ((top,_), _) = (Err.error 0 "Function cannot be declared at top level"; raise LevelError)
   | simpleVar ((_, _), top) = (Err.error 0 "Function cannot be declared at top level"; raise LevelError)
   | simpleVar ((decLevel, acc),uselevel) = Ex(Frame.exp(acc)(getFrame(decLevel, useLevel)))

					      
fun transSTRING (str : string) =
    let fun isString (F.PROC _) = false
	  | isString (F.STRING (l, s)) = str = s
	val str' = List.find(isString) (!fragments)
    in case str' of
	   SOME (F.STRING(l, s)) => Ex(T.NAME l)
	 | NONE => let
	             val lab = Temp.newlabel ()
		   in (fragments := F.STRING (lab, str) :: (!fragments);
		       Ex(T.NAME lab)) 
		   end
    end					      
					      
 fun callExp (callerLevel, top, label, explist) = (Err.error 0 "Function cannot be declared at top level"; raise LevelError)
   | callExp (callerLevel, level, label, explist) =
     let val staticLink = getFrame(level, callerLevel)
	 val unExdExplist = map unEx explist
     in
	 Ex(Tree.CALL(Tree.NAME(label), staticLink::unExdExplist))
     end
	 
 fun procEntryExit ({level=top, body=body})= (Err.error 0 "Function cannot be declared at top level"; raise LevelError)
   | procEntryExit ({level={parent=parent, frame=frame, unique=uniq}, body=b}) = 
     fragList := (!fragList @ [(Frame.PROC{body=Tree.MOVE(Tree.TEMP(Frame.RV), unEx(body)), frame=frame})])
     
end
