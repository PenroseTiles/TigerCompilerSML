structure Main =
struct
  fun print_intermediate filename =
    let
      val ast = Parse.parse filename
      val _ = FindEscape.findEscape ast
    in  
      Semant.transProg ast
    end
end
