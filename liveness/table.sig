signature TABLE =
sig
   type key
   type 'a table
   val empty : 'a table
   val enter : 'a table * key * 'a -> 'a table
   val look  : 'a table * key -> 'a option

   (* Added to easily facilitate set operations on tables *)
   val union : (('a * 'a) -> 'a) -> 'a table * 'a table -> 'a table
   val listItemsi : 'a table -> (int * 'a) list
   val difference : 'a table * 'a table -> 'a table
end

