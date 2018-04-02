functor IntMapTable (type key
                     val getInt: key -> int) : TABLE =
struct
  type key=key
  type 'a table = 'a IntBinaryMap.map
  val empty = IntBinaryMap.empty
  fun enter(t,k,a) = IntBinaryMap.insert(t,getInt k,a)
  fun look(t,k) = IntBinaryMap.find(t,getInt k)
  fun delete(t, k) = IntBinaryMap.remove(t, getInt k)
  fun union f (t1, t2) = IntBinaryMap.unionWith f (t1, t2)
  fun listItems(t) = IntBinaryMap.listItems(t)
  fun listItemsi(t) = IntBinaryMap.listItemsi(t)
  fun filter f (t) = IntBinaryMap.filter f (t)
  fun difference(t1, t2) =
      let
        val t2Keys = map (fn (k, a) => k) (listItemsi(t2))
        fun differ([], t) = t
          | differ(k::ks, t) =
            let 
              val newTable =
                  case IntBinaryMap.find(t1, k)
                   of SOME(_) =>
                      let val (keep, drop) = IntBinaryMap.remove(t, k)
                      in keep end
                    | NONE => t
            in
              differ(ks, newTable)
            end
      in
        differ(t2Keys, t1)
      end
      
end
