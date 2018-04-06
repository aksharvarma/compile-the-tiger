functor IntMapTable (type key
                     val getInt: key -> int) : TABLE =
struct
  type key=key
  type 'a table = 'a IntBinaryMap.map
  val empty = IntBinaryMap.empty
  fun enter(t,k,a) = IntBinaryMap.insert(t,getInt k,a)
  fun look(t,k) = IntBinaryMap.find(t,getInt k)

  (* Added to easily facilitate set operations on tables *)

  (* union : (('a * 'a) -> 'a) -> 'a table * 'a table -> 'a table
   *
   * Unions t1 with t2. The first argument is a function that describes what the
   * resulting value should be when there is an element in both t1 and t2 with
   * the same key.
   * Simply passes through the given arguments to IntBinaryMap.unionWith
   *)
  fun union f (t1, t2) = IntBinaryMap.unionWith f (t1, t2)

  (* listItemsi : 'a table -> (int * 'a) list
   *
   * Returns a list of all the key-value pairs in the given table.
   * Simply passes the table through to IntBinaryMap.listItemsi
   *)
  fun listItemsi(t) = IntBinaryMap.listItemsi(t)

  (* difference : 'a table * 'a table -> 'a table
   *
   * Returns a table representing the equivalent set difference: t1 - t2
   * Iterates through the keys of t2. If there is a key in t2 that is also
   * present in t1, then remove it from t1.
   *)
  fun difference(t1, t2) =
      let
        (* Get a list of keys in t2 *)
        val t2Keys = map (fn (k, a) => k) (listItemsi(t2))

        (* differ: key list * 'a table -> 'a table
         *
         * Given a list of keys, remove them from the given table if they are
         * present there. Return the updated table.
         *)
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
