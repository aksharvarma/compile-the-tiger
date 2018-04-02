signature TABLE =
sig
   type key
   type 'a table
   val empty : 'a table
   val enter : 'a table * key * 'a -> 'a table
   val look  : 'a table * key -> 'a option
   val delete : 'a table * key -> 'a table * 'a
   val union : (('a * 'a) -> 'a) -> 'a table * 'a table -> 'a table
   val listItems : 'a table -> 'a list
   val listItemsi : 'a table -> (int * 'a) list
   val filter : ('a -> bool) -> 'a table -> 'a table
   val difference : 'a table * 'a table -> 'a table
end

