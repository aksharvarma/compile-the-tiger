functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\017\000\003\000\016\000\004\000\015\000\008\000\014\000\
\\009\000\048\000\016\000\013\000\029\000\012\000\032\000\011\000\
\\033\000\010\000\036\000\009\000\040\000\008\000\041\000\007\000\000\000\
\\001\000\002\000\017\000\003\000\016\000\004\000\015\000\008\000\014\000\
\\016\000\013\000\029\000\012\000\032\000\011\000\033\000\010\000\
\\036\000\009\000\038\000\080\000\040\000\008\000\041\000\007\000\000\000\
\\001\000\002\000\017\000\003\000\016\000\004\000\015\000\008\000\014\000\
\\016\000\013\000\029\000\012\000\032\000\011\000\033\000\010\000\
\\036\000\009\000\040\000\008\000\041\000\007\000\000\000\
\\001\000\002\000\042\000\000\000\
\\001\000\002\000\053\000\000\000\
\\001\000\002\000\060\000\000\000\
\\001\000\002\000\061\000\000\000\
\\001\000\002\000\062\000\000\000\
\\001\000\002\000\073\000\000\000\
\\001\000\002\000\073\000\013\000\072\000\000\000\
\\001\000\002\000\100\000\012\000\099\000\028\000\098\000\000\000\
\\001\000\002\000\102\000\000\000\
\\001\000\002\000\119\000\000\000\
\\001\000\002\000\124\000\000\000\
\\001\000\002\000\128\000\000\000\
\\001\000\005\000\094\000\009\000\093\000\000\000\
\\001\000\006\000\083\000\027\000\082\000\000\000\
\\001\000\006\000\114\000\000\000\
\\001\000\006\000\123\000\019\000\122\000\000\000\
\\001\000\007\000\068\000\009\000\067\000\000\000\
\\001\000\007\000\068\000\038\000\095\000\000\000\
\\001\000\007\000\070\000\009\000\069\000\015\000\033\000\016\000\032\000\
\\017\000\031\000\018\000\030\000\019\000\029\000\020\000\028\000\
\\021\000\027\000\022\000\026\000\023\000\025\000\024\000\024\000\
\\025\000\023\000\026\000\022\000\000\000\
\\001\000\007\000\070\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\038\000\096\000\000\000\
\\001\000\008\000\084\000\000\000\
\\001\000\009\000\113\000\000\000\
\\001\000\011\000\077\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\000\000\
\\001\000\011\000\092\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\000\000\
\\001\000\013\000\090\000\000\000\
\\001\000\013\000\120\000\000\000\
\\001\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\
\\030\000\066\000\000\000\
\\001\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\
\\034\000\105\000\000\000\
\\001\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\
\\035\000\065\000\000\000\
\\001\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\
\\035\000\125\000\000\000\
\\001\000\019\000\081\000\000\000\
\\001\000\019\000\091\000\000\000\
\\001\000\019\000\131\000\000\000\
\\001\000\027\000\064\000\000\000\
\\001\000\027\000\112\000\000\000\
\\001\000\037\000\041\000\042\000\040\000\043\000\039\000\044\000\038\000\000\000\
\\001\000\037\000\059\000\000\000\
\\001\000\038\000\063\000\000\000\
\\001\000\039\000\108\000\000\000\
\\001\000\039\000\110\000\000\000\
\\135\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\136\000\010\000\020\000\014\000\019\000\027\000\018\000\000\000\
\\137\000\000\000\
\\138\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\139\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\
\\031\000\106\000\000\000\
\\151\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\152\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\008\000\051\000\010\000\050\000\012\000\049\000\000\000\
\\159\000\000\000\
\\160\000\000\000\
\\161\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\162\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\163\000\002\000\017\000\003\000\016\000\004\000\015\000\008\000\014\000\
\\016\000\013\000\029\000\012\000\032\000\011\000\033\000\010\000\
\\036\000\009\000\040\000\008\000\041\000\007\000\000\000\
\\164\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\165\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\005\000\117\000\015\000\033\000\016\000\032\000\017\000\031\000\
\\018\000\030\000\019\000\029\000\020\000\028\000\021\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\000\000\
\\181\000\000\000\
\\182\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\183\000\042\000\040\000\043\000\039\000\044\000\038\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\002\000\104\000\000\000\
\\192\000\005\000\129\000\000\000\
\\193\000\000\000\
\\194\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\195\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\196\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\\197\000\015\000\033\000\016\000\032\000\017\000\031\000\018\000\030\000\
\\019\000\029\000\020\000\028\000\021\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\000\000\
\"
val actionRowNumbers =
"\003\000\058\000\057\000\045\000\
\\044\000\052\000\049\000\039\000\
\\004\000\003\000\003\000\003\000\
\\001\000\054\000\053\000\067\000\
\\003\000\005\000\003\000\003\000\
\\086\000\085\000\084\000\083\000\
\\082\000\081\000\080\000\079\000\
\\078\000\075\000\077\000\076\000\
\\092\000\092\000\092\000\040\000\
\\006\000\007\000\008\000\041\000\
\\037\000\032\000\030\000\055\000\
\\020\000\022\000\050\000\010\000\
\\003\000\072\000\047\000\068\000\
\\026\000\056\000\095\000\094\000\
\\093\000\002\000\034\000\017\000\
\\024\000\051\000\003\000\003\000\
\\003\000\066\000\003\000\065\000\
\\003\000\028\000\087\000\035\000\
\\027\000\016\000\073\000\069\000\
\\021\000\023\000\062\000\011\000\
\\003\000\012\000\100\000\031\000\
\\048\000\059\000\071\000\070\000\
\\088\000\003\000\042\000\046\000\
\\003\000\064\000\063\000\096\000\
\\043\000\100\000\097\000\105\000\
\\038\000\025\000\018\000\003\000\
\\003\000\089\000\003\000\074\000\
\\013\000\029\000\003\000\019\000\
\\014\000\033\000\060\000\009\000\
\\091\000\099\000\098\000\106\000\
\\003\000\015\000\101\000\003\000\
\\090\000\103\000\036\000\100\000\
\\061\000\003\000\102\000\104\000\
\\000\000"
val gotoT =
"\
\\001\000\132\000\002\000\004\000\003\000\003\000\010\000\002\000\
\\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\000\000\
\\013\000\035\000\014\000\034\000\017\000\033\000\018\000\032\000\000\000\
\\000\000\
\\002\000\041\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\002\000\042\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\002\000\043\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\002\000\045\000\003\000\003\000\005\000\044\000\010\000\002\000\
\\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\050\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\000\000\
\\002\000\052\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\002\000\053\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\054\000\014\000\034\000\017\000\033\000\018\000\032\000\000\000\
\\013\000\055\000\014\000\034\000\017\000\033\000\018\000\032\000\000\000\
\\013\000\056\000\014\000\034\000\017\000\033\000\018\000\032\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\019\000\000\000\
\\009\000\019\000\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\011\000\069\000\000\000\
\\002\000\072\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\002\000\074\000\003\000\003\000\008\000\073\000\010\000\002\000\
\\012\000\001\000\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\009\000\019\000\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\077\000\003\000\003\000\005\000\076\000\010\000\002\000\
\\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\083\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\002\000\084\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\002\000\085\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\000\000\
\\002\000\086\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\000\000\
\\002\000\087\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\015\000\095\000\000\000\
\\002\000\099\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\000\000\
\\016\000\101\000\000\000\
\\009\000\019\000\000\000\
\\009\000\019\000\000\000\
\\009\000\019\000\000\000\
\\009\000\019\000\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\002\000\105\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\107\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\109\000\000\000\
\\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\113\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\002\000\114\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\009\000\019\000\000\000\
\\002\000\116\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\000\000\
\\002\000\119\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\009\000\019\000\000\000\
\\009\000\019\000\000\000\
\\011\000\124\000\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\000\000\
\\009\000\019\000\000\000\
\\002\000\125\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\128\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\000\000\
\\009\000\019\000\000\000\
\\000\000\
\\016\000\130\000\000\000\
\\009\000\019\000\000\000\
\\002\000\131\000\003\000\003\000\010\000\002\000\012\000\001\000\000\000\
\\000\000\
\\009\000\019\000\000\000\
\\000\000\
\"
val numstates = 133
val numrules = 63
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 31) => true | (T 32) => true | (T 33) => true | (T 39) => true
 | (T 35) => true | (T 36) => true | (T 37) => true | (T 41) => true
 | (T 42) => true | (T 43) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 34) => true | (T 38) => true
 | (T 40) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 29))::
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "ARRAY"
  | (T 28) => "IF"
  | (T 29) => "THEN"
  | (T 30) => "ELSE"
  | (T 31) => "WHILE"
  | (T 32) => "FOR"
  | (T 33) => "TO"
  | (T 34) => "DO"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | (T 38) => "OF"
  | (T 39) => "BREAK"
  | (T 40) => "NIL"
  | (T 41) => "FUNCTION"
  | (T 42) => "VAR"
  | (T 43) => "TYPE"
  | (T 44) => "UMINUS"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID expr1, expr1left, expr1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
expr1 = expr1 ()
 in (print("always print this\n"))
end; ()))
 in ( LrTable.NT 0, ( result, expr1left, expr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 (lvalue as lvalue1) = lvalue1 ()
 in (print("expr -> lvalue\n"))
end; ()))
 in ( LrTable.NT 1, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 2, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
funparam1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1
 = ID1 ()
 val  funparam1 = funparam1 ()
 in (print("expr -> funname(foo,...)\n"))
end; ()))
 in ( LrTable.NT 1, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.ntVOID expr1, _, expr1right)) :: _ :: ( _, (
 MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  (lvalue as lvalue1) = 
lvalue1 ()
 val  (expr as expr1) = expr1 ()
 in (print("expr -> lvalue := expr \n"))
end; ()))
 in ( LrTable.NT 1, ( result, lvalue1left, expr1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ntVOID expr2, _, expr2right)) :: _ :: ( _, (
 MlyValue.ntVOID expr1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671
)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  expr1 = 
expr1 ()
 val  expr2 = expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, WHILE1left, expr2right), rest671)
end
|  ( 5, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val 
 result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 1, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 6, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 7, ( ( _, ( _, _, END1right)) :: _ :: ( _, ( _, LET1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 8, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 1, ( result, NIL1left, NIL1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  INT1 = INT1
 ()
 in (print("Integer\n"))
end; ()))
 in ( LrTable.NT 1, ( result, INT1left, INT1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 STRING1 = STRING1 ()
 in (print("string\n"))
end; ()))
 in ( LrTable.NT 1, ( result, STRING1left, STRING1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ntVOID expr1, _, expr1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  expr1 = expr1 ()
 in (print("Unary minus\n"))
end; ()))
 in ( LrTable.NT 1, ( result, MINUS1left, expr1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ntVOID expr2, _, expr2right)) :: ( _, ( 
MlyValue.ntVOID op1, _, _)) :: ( _, ( MlyValue.ntVOID expr1, expr1left
, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  expr1 = expr1 ()
 val  op1 = op1 ()
 val  expr2 = expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, expr1left, expr2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ntVOID record1, record1left, record1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 (record as record1) = record1 ()
 in (print("expr -> record\n"))
end; ()))
 in ( LrTable.NT 1, ( result, record1left, record1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ntVOID array1, array1left, array1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (
array as array1) = array1 ()
 in (print("expr -> array creation\n"))
end; ()))
 in ( LrTable.NT 1, ( result, array1left, array1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ntVOID expr2, _, expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID expr1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  expr1 = 
expr1 ()
 val  expr2 = expr2 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, IF1left, expr2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ntVOID expr3, _, expr3right)) :: _ :: ( _, 
( MlyValue.ntVOID expr2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID expr1,
 _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 val  expr3 = expr3 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, IF1left, expr3right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ntVOID expr3, _, expr3right)) :: _ :: ( _, 
( MlyValue.ntVOID expr2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID expr1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FOR1left,
 _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  ID1 = ID1 ()
 val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 val  expr3 = expr3 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, FOR1left, expr3right), rest671)
end
|  ( 18, ( ( _, ( _, _, END1right)) :: _ :: ( _, ( MlyValue.ntVOID 
decs1, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  decs1 = decs1 ()
 in (print("expr -> LET with 0 expr\n"))
end; ()))
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 19, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.ntVOID expr1, _
, _)) :: _ :: ( _, ( MlyValue.ntVOID decs1, _, _)) :: ( _, ( _, 
LET1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  decs1 = decs1 ()
 val  (expr as expr1) = expr1 ()
 in (print("expr -> LET with 1 expr\n"))
end; ()))
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 20, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.ntVOID exprSeq1
, _, _)) :: _ :: ( _, ( MlyValue.ntVOID decs1, _, _)) :: ( _, ( _, 
LET1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  decs1 = decs1 ()
 val  (exprSeq as exprSeq1) = exprSeq1 ()
 in (print("expr -> LET with exprSeq\n"))
end; ()))
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 21, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID expr1
, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  expr1 = expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 22, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
exprSeq1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  exprSeq1 = exprSeq1
 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = 
ID1 ()
 in (print("lvalue -> ID\n"))
end; ()))
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  (lvalue as lvalue1) = 
lvalue1 ()
 val  (ID as ID1) = ID1 ()
 in (print("lvalue -> lvalue.ID\n"))
end; ()))
 in ( LrTable.NT 2, ( result, lvalue1left, ID1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.ntVOID expr1
, _, _)) :: _ :: ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (
lvalue as lvalue1) = lvalue1 ()
 val  (expr as expr1) = expr1 ()
 in (print("lvalue -> lvalue[expr]\n"))
end; ()))
 in ( LrTable.NT 2, ( result, lvalue1left, RBRACK1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ntVOID expr2, _, expr2right)) :: _ :: ( _, 
( MlyValue.ntVOID expr1, expr1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  (expr as expr1) = expr1
 ()
 val  expr2 = expr2 ()
 in (print("expr -> expr ; expr\n"))
end; ()))
 in ( LrTable.NT 4, ( result, expr1left, expr2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.ntVOID expr1, _, expr1right)) :: _ :: ( _, 
( MlyValue.ntVOID exprSeq1, exprSeq1left, _)) :: rest671)) => let val 
 result = MlyValue.ntVOID (fn _ => ( let val  (exprSeq as exprSeq1) = 
exprSeq1 ()
 val  (expr as expr1) = expr1 ()
 in (print("expr -> exprSeq ; expr\n"))
end; ()))
 in ( LrTable.NT 4, ( result, exprSeq1left, expr1right), rest671)
end
|  ( 28, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 29, ( ( _, ( MlyValue.ntVOID expr1, expr1left, expr1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
expr1 = expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, expr1left, expr1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.ntVOID expr1, _, expr1right)) :: _ :: ( _, 
( MlyValue.ntVOID funparam1, funparam1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  funparam1 = 
funparam1 ()
 val  expr1 = expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, funparam1left, expr1right), rest671)
end
|  ( 31, ( ( _, ( _, TIMES1left, TIMES1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, TIMES1left, TIMES1right), rest671)
end
|  ( 32, ( ( _, ( _, PLUS1left, PLUS1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, PLUS1left, PLUS1right), rest671)
end
|  ( 33, ( ( _, ( _, MINUS1left, MINUS1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, MINUS1left, MINUS1right), rest671)
end
|  ( 34, ( ( _, ( _, DIVIDE1left, DIVIDE1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, DIVIDE1left, DIVIDE1right), rest671)
end
|  ( 35, ( ( _, ( _, EQ1left, EQ1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, EQ1left, EQ1right), rest671)
end
|  ( 36, ( ( _, ( _, NEQ1left, NEQ1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, NEQ1left, NEQ1right), rest671)
end
|  ( 37, ( ( _, ( _, LT1left, LT1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, LT1left, LT1right), rest671)
end
|  ( 38, ( ( _, ( _, LE1left, LE1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, LE1left, LE1right), rest671)
end
|  ( 39, ( ( _, ( _, GT1left, GT1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, GT1left, GT1right), rest671)
end
|  ( 40, ( ( _, ( _, GE1left, GE1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, GE1left, GE1right), rest671)
end
|  ( 41, ( ( _, ( _, AND1left, AND1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, AND1left, AND1right), rest671)
end
|  ( 42, ( ( _, ( _, OR1left, OR1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, OR1left, OR1right), rest671)
end
|  ( 43, ( ( _, ( _, _, RBRACE1right)) :: _ :: ( _, ( MlyValue.ID ID1,
 ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 44, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID 
recordFields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1
 = ID1 ()
 val  recordFields1 = recordFields1 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.ntVOID expr1, _, expr1right)) :: _ :: ( _, 
( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  expr1 = expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, ID1left, expr1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.ntVOID recordFields1, _, recordFields1right
)) :: _ :: ( _, ( MlyValue.ntVOID expr1, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  expr1 = expr1 ()
 val  recordFields1 = recordFields1 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, ID1left, recordFields1right), rest671)

end
|  ( 47, ( ( _, ( MlyValue.ntVOID expr2, _, expr2right)) :: _ :: _ :: 
( _, ( MlyValue.ntVOID expr1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  ID1 = ID1 ()
 val  (expr as expr1) = expr1 ()
 val  expr2 = expr2 ()
 in (print("array -> id[expr] of expr\n"))
end; ()))
 in ( LrTable.NT 11, ( result, ID1left, expr2right), rest671)
end
|  ( 48, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 49, ( ( _, ( MlyValue.ntVOID decs1, _, decs1right)) :: ( _, ( 
MlyValue.ntVOID tydec1, tydec1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  tydec1 = tydec1 ()
 val  decs1 = decs1 ()
 in ()
end; ()))
 in ( LrTable.NT 12, ( result, tydec1left, decs1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.ntVOID decs1, _, decs1right)) :: ( _, ( 
MlyValue.ntVOID vardec1, vardec1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  vardec1 = vardec1 ()
 val  decs1 = decs1 ()
 in ()
end; ()))
 in ( LrTable.NT 12, ( result, vardec1left, decs1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.ntVOID decs1, _, decs1right)) :: ( _, ( 
MlyValue.ntVOID fundec1, fundec1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  fundec1 = fundec1 ()
 val  decs1 = decs1 ()
 in ()
end; ()))
 in ( LrTable.NT 12, ( result, fundec1left, decs1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ntVOID ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ty1 = ty1 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, TYPE1left, ty1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, ID1left, ID1right), rest671)
end
|  ( 54, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID 
tyfields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  tyfields1 = 
tyfields1 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 55, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, 
ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 56, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 57, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in ()
end; ()))
 in ( LrTable.NT 15, ( result, ID1left, ID2right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ntVOID tyfields1, _, tyfields1right)) :: _
 :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  tyfields1 = tyfields1 ()
 in ()
end; ()))
 in ( LrTable.NT 15, ( result, ID1left, tyfields1right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.ntVOID expr1, _, expr1right)) :: _ :: _ :: 
( _, ( MlyValue.ntVOID tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID 
ID1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  tyfields1 = tyfields1 ()
 val  expr1 = expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 17, ( result, FUNCTION1left, expr1right), rest671)

end
|  ( 60, ( ( _, ( MlyValue.ntVOID expr1, _, expr1right)) :: _ :: ( _, 
( MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID 
tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  ID1 = ID1 ()
 val  tyfields1 = tyfields1 ()
 val  ID2 = ID2 ()
 val  expr1 = expr1 ()
 in ()
end; ()))
 in ( LrTable.NT 17, ( result, FUNCTION1left, expr1right), rest671)

end
|  ( 61, ( ( _, ( MlyValue.ntVOID expr1, _, expr1right)) :: _ :: ( _, 
( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  (expr as expr1) = expr1 ()
 in (print("vardec -> var id := expr\n"))
end; ()))
 in ( LrTable.NT 16, ( result, VAR1left, expr1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.ntVOID expr1, _, expr1right)) :: _ :: ( _, 
( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: (
 _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (expr as expr1) = expr1 ()
 in (print("vardec -> var id:id := expr\n"))
end; ()))
 in ( LrTable.NT 16, ( result, VAR1left, expr1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
