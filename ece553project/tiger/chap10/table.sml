signature TABLE =
sig
   type key
   type 'a table
   val empty : 'a table
   val enter : 'a table * key * 'a -> 'a table
   val look  : 'a table * key -> 'a option
   val listItems : 'a table  -> 'a list
   val listKeys : 'a table -> key list
end


functor IntMapTable (type key
		     val getInt: key -> int) =
struct
  type key=key
  type 'a table = 'a IntBinaryMap.map
  val empty = IntBinaryMap.empty
  fun enter(t,k,a) = IntBinaryMap.insert(t,getInt k,a)
  fun look(t,k) = IntBinaryMap.find(t,getInt k)
  fun listItems(t) = IntBinaryMap.listItems(t)
  fun listKeys(t) = IntBinaryMap.listKeys(t)					   
end
