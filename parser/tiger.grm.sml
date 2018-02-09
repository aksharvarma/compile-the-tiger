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
structure A = Absyn
val sym = Symbol.symbolize
type symbol = Symbol.symbol
val name = Symbol.name
type recordFieldsT = ((symbol * A.exp * A.pos) list)
			 

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\158\000\005\000\158\000\007\000\158\000\009\000\158\000\
\\011\000\158\000\013\000\158\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\025\000\158\000\026\000\158\000\
\\030\000\158\000\031\000\158\000\034\000\158\000\035\000\158\000\
\\037\000\158\000\038\000\158\000\042\000\158\000\043\000\158\000\
\\044\000\158\000\000\000\
\\001\000\001\000\159\000\005\000\159\000\007\000\159\000\009\000\159\000\
\\011\000\159\000\013\000\159\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\025\000\159\000\026\000\159\000\
\\030\000\159\000\031\000\159\000\034\000\159\000\035\000\159\000\
\\037\000\159\000\038\000\159\000\042\000\159\000\043\000\159\000\
\\044\000\159\000\000\000\
\\001\000\001\000\160\000\005\000\160\000\007\000\160\000\009\000\160\000\
\\011\000\160\000\013\000\160\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\025\000\160\000\026\000\160\000\
\\030\000\160\000\031\000\160\000\034\000\160\000\035\000\160\000\
\\037\000\160\000\038\000\160\000\042\000\160\000\043\000\160\000\
\\044\000\160\000\000\000\
\\001\000\001\000\161\000\005\000\161\000\007\000\161\000\009\000\161\000\
\\011\000\161\000\013\000\161\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\025\000\161\000\026\000\161\000\
\\030\000\161\000\031\000\161\000\034\000\161\000\035\000\161\000\
\\037\000\161\000\038\000\161\000\042\000\161\000\043\000\161\000\
\\044\000\161\000\000\000\
\\001\000\001\000\162\000\005\000\162\000\007\000\162\000\009\000\162\000\
\\011\000\162\000\013\000\162\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\025\000\162\000\026\000\162\000\
\\030\000\162\000\031\000\162\000\034\000\162\000\035\000\162\000\
\\037\000\162\000\038\000\162\000\042\000\162\000\043\000\162\000\
\\044\000\162\000\000\000\
\\001\000\001\000\163\000\005\000\163\000\007\000\163\000\009\000\163\000\
\\011\000\163\000\013\000\163\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\025\000\163\000\026\000\163\000\
\\030\000\163\000\031\000\163\000\034\000\163\000\035\000\163\000\
\\037\000\163\000\038\000\163\000\042\000\163\000\043\000\163\000\
\\044\000\163\000\000\000\
\\001\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\009\000\039\000\016\000\012\000\029\000\011\000\032\000\010\000\
\\033\000\009\000\036\000\008\000\040\000\007\000\041\000\006\000\000\000\
\\001\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\029\000\011\000\032\000\010\000\033\000\009\000\
\\036\000\008\000\038\000\088\000\040\000\007\000\041\000\006\000\000\000\
\\001\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\029\000\011\000\032\000\010\000\033\000\009\000\
\\036\000\008\000\040\000\007\000\041\000\006\000\000\000\
\\001\000\002\000\033\000\000\000\
\\001\000\002\000\044\000\000\000\
\\001\000\002\000\076\000\013\000\075\000\000\000\
\\001\000\002\000\083\000\000\000\
\\001\000\002\000\084\000\000\000\
\\001\000\002\000\085\000\000\000\
\\001\000\002\000\108\000\000\000\
\\001\000\002\000\115\000\012\000\114\000\028\000\113\000\000\000\
\\001\000\002\000\117\000\000\000\
\\001\000\002\000\132\000\000\000\
\\001\000\002\000\137\000\000\000\
\\001\000\002\000\138\000\000\000\
\\001\000\002\000\141\000\000\000\
\\001\000\002\000\144\000\000\000\
\\001\000\005\000\095\000\013\000\094\000\000\000\
\\001\000\005\000\099\000\009\000\098\000\000\000\
\\001\000\005\000\128\000\009\000\127\000\000\000\
\\001\000\005\000\128\000\013\000\133\000\000\000\
\\001\000\006\000\102\000\027\000\101\000\000\000\
\\001\000\006\000\129\000\000\000\
\\001\000\006\000\136\000\019\000\135\000\000\000\
\\001\000\006\000\142\000\000\000\
\\001\000\007\000\071\000\009\000\070\000\000\000\
\\001\000\007\000\071\000\038\000\104\000\000\000\
\\001\000\007\000\073\000\009\000\072\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\019\000\027\000\020\000\026\000\
\\021\000\025\000\022\000\024\000\023\000\023\000\024\000\022\000\
\\025\000\021\000\026\000\020\000\000\000\
\\001\000\007\000\073\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\025\000\021\000\
\\026\000\020\000\038\000\105\000\000\000\
\\001\000\008\000\103\000\000\000\
\\001\000\011\000\080\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\025\000\021\000\
\\026\000\020\000\000\000\
\\001\000\011\000\097\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\025\000\021\000\
\\026\000\020\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\030\000\069\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\034\000\106\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\035\000\068\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\035\000\130\000\000\000\
\\001\000\019\000\096\000\000\000\
\\001\000\019\000\100\000\000\000\
\\001\000\019\000\122\000\000\000\
\\001\000\019\000\143\000\000\000\
\\001\000\027\000\067\000\000\000\
\\001\000\027\000\126\000\000\000\
\\001\000\037\000\066\000\042\000\065\000\043\000\064\000\044\000\063\000\000\000\
\\001\000\039\000\124\000\000\000\
\\147\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\148\000\010\000\019\000\014\000\018\000\027\000\017\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\\151\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\017\000\029\000\018\000\028\000\000\000\
\\156\000\017\000\029\000\018\000\028\000\000\000\
\\157\000\000\000\
\\164\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\000\000\
\\165\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\171\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\031\000\107\000\000\000\
\\172\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\173\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\174\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\180\000\008\000\042\000\010\000\041\000\012\000\040\000\000\000\
\\181\000\000\000\
\\182\000\039\000\110\000\000\000\
\\183\000\000\000\
\\184\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\185\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\186\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\029\000\011\000\032\000\010\000\033\000\009\000\
\\036\000\008\000\040\000\007\000\041\000\006\000\000\000\
\\187\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\188\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\192\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\193\000\000\000\
\\194\000\044\000\063\000\000\000\
\\195\000\000\000\
\\196\000\042\000\065\000\000\000\
\\197\000\000\000\
\\198\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\000\000\
\\203\000\002\000\119\000\000\000\
\\204\000\000\000\
\\205\000\000\000\
\\206\000\000\000\
\\207\000\000\000\
\\208\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\209\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\210\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\211\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\"
val actionRowNumbers =
"\009\000\064\000\052\000\051\000\
\\053\000\073\000\091\000\010\000\
\\009\000\009\000\009\000\007\000\
\\055\000\054\000\078\000\009\000\
\\011\000\009\000\009\000\009\000\
\\009\000\009\000\009\000\009\000\
\\009\000\009\000\009\000\009\000\
\\009\000\009\000\049\000\047\000\
\\041\000\039\000\057\000\032\000\
\\034\000\065\000\012\000\009\000\
\\084\000\068\000\079\000\037\000\
\\063\000\062\000\006\000\005\000\
\\004\000\003\000\002\000\001\000\
\\061\000\058\000\060\000\059\000\
\\094\000\104\000\093\000\092\000\
\\095\000\013\000\014\000\015\000\
\\008\000\009\000\009\000\009\000\
\\067\000\009\000\066\000\009\000\
\\024\000\087\000\043\000\038\000\
\\025\000\085\000\081\000\105\000\
\\096\000\044\000\028\000\036\000\
\\033\000\035\000\074\000\040\000\
\\071\000\069\000\083\000\082\000\
\\088\000\016\000\009\000\080\000\
\\056\000\009\000\017\000\009\000\
\\018\000\101\000\076\000\075\000\
\\009\000\009\000\045\000\089\000\
\\009\000\086\000\097\000\050\000\
\\101\000\098\000\108\000\048\000\
\\026\000\029\000\042\000\070\000\
\\009\000\077\000\019\000\027\000\
\\009\000\030\000\020\000\021\000\
\\009\000\090\000\100\000\099\000\
\\109\000\009\000\022\000\031\000\
\\102\000\072\000\106\000\046\000\
\\023\000\009\000\103\000\107\000\
\\000\000"
val gotoT =
"\
\\001\000\144\000\002\000\003\000\003\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\030\000\000\000\
\\000\000\
\\002\000\032\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\033\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\034\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\036\000\003\000\002\000\004\000\035\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\041\000\003\000\002\000\006\000\001\000\000\000\
\\000\000\
\\002\000\043\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\044\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\045\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\046\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\047\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\048\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\049\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\050\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\051\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\052\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\053\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\054\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\055\000\003\000\002\000\006\000\001\000\000\000\
\\009\000\060\000\010\000\059\000\013\000\058\000\014\000\057\000\
\\015\000\056\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\072\000\000\000\
\\002\000\075\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\077\000\003\000\002\000\005\000\076\000\006\000\001\000\000\000\
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
\\000\000\
\\000\000\
\\000\000\
\\014\000\079\000\000\000\
\\000\000\
\\000\000\
\\009\000\080\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\085\000\003\000\002\000\004\000\084\000\006\000\001\000\000\000\
\\002\000\087\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\088\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\089\000\003\000\002\000\006\000\001\000\000\000\
\\000\000\
\\002\000\090\000\003\000\002\000\006\000\001\000\000\000\
\\000\000\
\\002\000\091\000\003\000\002\000\006\000\001\000\000\000\
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
\\002\000\107\000\003\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\109\000\003\000\002\000\006\000\001\000\000\000\
\\011\000\110\000\000\000\
\\002\000\114\000\003\000\002\000\006\000\001\000\000\000\
\\000\000\
\\012\000\116\000\000\000\
\\000\000\
\\000\000\
\\002\000\118\000\003\000\002\000\006\000\001\000\000\000\
\\002\000\119\000\003\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\121\000\003\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\123\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\129\000\003\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\132\000\003\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\137\000\003\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\138\000\003\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\143\000\003\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 145
val numrules = 65
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
type pos = Absyn.pos
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | fundecs of unit ->  (A.fundec list)
 | fundec of unit ->  (A.fundec) | vardec of unit ->  (A.dec)
 | tyfields of unit ->  (A.field list) | ty of unit ->  (A.ty)
 | tydecs of unit ->  ({ name:symbol,ty:A.ty,pos:A.pos }  list)
 | tydec of unit ->  ({ name:symbol,ty:A.ty,pos:A.pos } )
 | decs of unit ->  (A.dec list)
 | recordFields of unit ->  (recordFieldsT)
 | record of unit ->  (A.exp) | funparam of unit ->  (A.exp list)
 | exprSeq of unit ->  ( ( A.exp * A.pos )  list)
 | lvalue of unit ->  (A.var) | expr of unit ->  (A.exp)
 | program of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
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
(nil
,nil
 $$ (T 8))::
(nil
,nil
 $$ (T 1))::
(nil
,nil
 $$ (T 14))::
(nil
,nil
 $$ (T 18))::
(nil
 $$ (T 18),nil
 $$ (T 26))::
(nil
 $$ (T 26),nil
 $$ (T 18))::
(nil
 $$ (T 30) $$ (T 6),nil
 $$ (T 30))::
(nil
,nil
 $$ (T 37) $$ (T 2) $$ (T 36))::
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
  | (T 45) => "LOWERTHANOPHACK"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("arbit-string")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39)
 $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32)
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25)
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18)
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.expr expr1, expr1left, expr1right)) :: 
rest671)) => let val  result = MlyValue.program (fn _ => let val  (
expr as expr1) = expr1 ()
 in (expr)
end)
 in ( LrTable.NT 0, ( result, expr1left, expr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.lvalue lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.expr (fn _ => let val  (
lvalue as lvalue1) = lvalue1 ()
 in (A.VarExp(lvalue))
end)
 in ( LrTable.NT 1, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 2, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.expr (fn _ => (A.NilExp))
 in ( LrTable.NT 1, ( result, NIL1left, NIL1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.expr (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (A.IntExp(INT))
end)
 in ( LrTable.NT 1, ( result, INT1left, INT1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left),
 STRING1right)) :: rest671)) => let val  result = MlyValue.expr (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp(STRING, STRINGleft))
end)
 in ( LrTable.NT 1, ( result, STRING1left, STRING1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.funparam 
funparam1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left),
 _)) :: rest671)) => let val  result = MlyValue.expr (fn _ => let val 
 (ID as ID1) = ID1 ()
 val  (funparam as funparam1) = funparam1 ()
 in (
A.CallExp({func=sym(ID),
					    args=funparam,
					    pos=IDleft})
)
end)
 in ( LrTable.NT 1, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, (
MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  (expr as expr1) = expr1 ()
 in (
A.OpExp({left=A.IntExp(0),
					  oper=A.MinusOp,
					  right=expr, pos=MINUSleft})
)
end)
 in ( LrTable.NT 1, ( result, MINUS1left, expr1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: ( _, ( _, 
TIMESleft, _)) :: ( _, ( MlyValue.expr expr1, expr1left, _)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => let val  expr1 =
 expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp({left=expr1,
				oper=A.TimesOp,
				right=expr2, pos=TIMESleft})
)
end)
 in ( LrTable.NT 1, ( result, expr1left, expr2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: ( _, ( _, 
PLUSleft, _)) :: ( _, ( MlyValue.expr expr1, expr1left, _)) :: rest671
)) => let val  result = MlyValue.expr (fn _ => let val  expr1 = expr1
 ()
 val  expr2 = expr2 ()
 in (
A.OpExp({left=expr1,
				oper=A.PlusOp,
				right=expr2, pos=PLUSleft})
)
end)
 in ( LrTable.NT 1, ( result, expr1left, expr2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: ( _, ( _, 
MINUSleft, _)) :: ( _, ( MlyValue.expr expr1, expr1left, _)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => let val  expr1 =
 expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp({left=expr1,
				oper=A.MinusOp,
				right=expr2, pos=MINUSleft})
)
end)
 in ( LrTable.NT 1, ( result, expr1left, expr2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: ( _, ( _, 
DIVIDEleft, _)) :: ( _, ( MlyValue.expr expr1, expr1left, _)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => let val  expr1 =
 expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp({left=expr1,
				oper=A.DivideOp,
				right=expr2, pos=DIVIDEleft})
)
end)
 in ( LrTable.NT 1, ( result, expr1left, expr2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: ( _, ( _, 
EQleft, _)) :: ( _, ( MlyValue.expr expr1, expr1left, _)) :: rest671))
 => let val  result = MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp({left=expr1,
				oper=A.EqOp,
				right=expr2, pos=EQleft}))

end)
 in ( LrTable.NT 1, ( result, expr1left, expr2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: ( _, ( _, 
NEQleft, _)) :: ( _, ( MlyValue.expr expr1, expr1left, _)) :: rest671)
) => let val  result = MlyValue.expr (fn _ => let val  expr1 = expr1
 ()
 val  expr2 = expr2 ()
 in (
A.OpExp({left=expr1,
				oper=A.NeqOp,
				right=expr2, pos=NEQleft}))

end)
 in ( LrTable.NT 1, ( result, expr1left, expr2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: ( _, ( _, 
LTleft, _)) :: ( _, ( MlyValue.expr expr1, expr1left, _)) :: rest671))
 => let val  result = MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp({left=expr1,
				oper=A.LtOp,
				right=expr2, pos=LTleft}))

end)
 in ( LrTable.NT 1, ( result, expr1left, expr2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: ( _, ( _, 
LEleft, _)) :: ( _, ( MlyValue.expr expr1, expr1left, _)) :: rest671))
 => let val  result = MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp({left=expr1,
				oper=A.LeOp,
				right=expr2, pos=LEleft}))

end)
 in ( LrTable.NT 1, ( result, expr1left, expr2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: ( _, ( _, 
GTleft, _)) :: ( _, ( MlyValue.expr expr1, expr1left, _)) :: rest671))
 => let val  result = MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp({left=expr1,
				oper=A.GtOp,
				right=expr2, pos=GTleft}))

end)
 in ( LrTable.NT 1, ( result, expr1left, expr2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: ( _, ( _, 
GEleft, _)) :: ( _, ( MlyValue.expr expr1, expr1left, _)) :: rest671))
 => let val  result = MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.OpExp({left=expr1,
			     oper=A.GeOp,
			     right=expr2, pos=GEleft})
)
end)
 in ( LrTable.NT 1, ( result, expr1left, expr2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: ( _, ( _, 
ANDleft, _)) :: ( _, ( MlyValue.expr expr1, expr1left, _)) :: rest671)
) => let val  result = MlyValue.expr (fn _ => let val  expr1 = expr1
 ()
 val  expr2 = expr2 ()
 in (
A.IfExp({test=expr1, then'=expr2, else'=SOME(A.IntExp(0)), pos=ANDleft})
)
end)
 in ( LrTable.NT 1, ( result, expr1left, expr2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: ( _, ( _, 
ORleft, _)) :: ( _, ( MlyValue.expr expr1, expr1left, _)) :: rest671))
 => let val  result = MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (
A.IfExp({test=expr1, then'=A.IntExp(1), else'=SOME(expr2), pos=ORleft})
)
end)
 in ( LrTable.NT 1, ( result, expr1left, expr2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.record record1, record1left, record1right))
 :: rest671)) => let val  result = MlyValue.expr (fn _ => let val  (
record as record1) = record1 ()
 in (record)
end)
 in ( LrTable.NT 1, ( result, record1left, record1right), rest671)
end
|  ( 20, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.expr (fn _ => (
A.SeqExp([])))
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 21, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr1, 
exprleft, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  
result = MlyValue.expr (fn _ => let val  (expr as expr1) = expr1 ()
 in (A.SeqExp([(expr, exprleft)]))
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 22, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exprSeq 
exprSeq1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.expr (fn _ => let val  (exprSeq as exprSeq1) =
 exprSeq1 ()
 in (A.SeqExp(exprSeq))
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
ASSIGNleft, _)) :: ( _, ( MlyValue.lvalue lvalue1, lvalue1left, _)) ::
 rest671)) => let val  result = MlyValue.expr (fn _ => let val  (
lvalue as lvalue1) = lvalue1 ()
 val  (expr as expr1) = expr1 ()
 in (A.AssignExp({var=lvalue, exp=expr, pos=ASSIGNleft}))
end)
 in ( LrTable.NT 1, ( result, lvalue1left, expr1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => let val  expr1 =
 expr1 ()
 val  expr2 = expr2 ()
 in (
A.IfExp({test=expr1,
							then'=expr2,
							else'=NONE,
							pos=IFleft})
)
end)
 in ( LrTable.NT 1, ( result, IF1left, expr2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.expr expr3, _, expr3right)) :: _ :: ( _, ( 
MlyValue.expr expr2, _, _)) :: _ :: ( _, ( MlyValue.expr expr1, _, _))
 :: ( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result
 = MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 val  expr3 = expr3 ()
 in (A.IfExp({test=expr1, then'=expr2, else'=SOME(expr3), pos=IFleft})
)
end)
 in ( LrTable.NT 1, ( result, IF1left, expr3right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _)
) :: rest671)) => let val  result = MlyValue.expr (fn _ => let val  
expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (A.WhileExp({test=expr1, body=expr2, pos=WHILEleft}))
end)
 in ( LrTable.NT 1, ( result, WHILE1left, expr2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.expr expr3, _, expr3right)) :: _ :: ( _, ( 
MlyValue.expr expr2, _, _)) :: _ :: ( _, ( MlyValue.expr expr1, _, _))
 :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as 
FOR1left), _)) :: rest671)) => let val  result = MlyValue.expr (fn _
 => let val  (ID as ID1) = ID1 ()
 val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 val  expr3 = expr3 ()
 in (
A.ForExp({var=sym(ID), escape=ref true, lo=expr1, hi=expr2, body=expr3, pos=FORleft})
)
end)
 in ( LrTable.NT 1, ( result, FOR1left, expr3right), rest671)
end
|  ( 28, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => (
A.BreakExp(BREAKleft)))
 in ( LrTable.NT 1, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 29, ( ( _, ( _, _, END1right)) :: _ :: ( _, ( MlyValue.decs decs1
, _, _)) :: ( _, ( _, (LETleft as LET1left), _)) :: rest671)) => let
 val  result = MlyValue.expr (fn _ => let val  (decs as decs1) = decs1
 ()
 in (A.LetExp({decs=decs, body=A.SeqExp([]),
				 pos=LETleft}))
end)
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 30, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.expr expr1, _,
 _)) :: _ :: ( _, ( MlyValue.decs decs1, _, _)) :: ( _, ( _, (LETleft
 as LET1left), _)) :: rest671)) => let val  result = MlyValue.expr (fn
 _ => let val  (decs as decs1) = decs1 ()
 val  (expr as expr1) = expr1 ()
 in (A.LetExp({decs=decs,
				      body=expr, pos=LETleft}))
end)
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 31, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.exprSeq 
exprSeq1, _, _)) :: _ :: ( _, ( MlyValue.decs decs1, _, _)) :: ( _, (
 _, (LETleft as LET1left), _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  (decs as decs1) = decs1 ()
 val  (exprSeq as exprSeq1) = exprSeq1 ()
 in (
A.LetExp({decs=decs,
					 body=A.SeqExp(exprSeq),
					 pos=LETleft})
)
end)
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: _ :: (
 _, ( MlyValue.expr expr1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (
IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  (ID as ID1) = ID1 ()
 val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (

	   A.ArrayExp({typ=sym(ID), size=expr1, init=expr2, pos=IDleft}))

end)
 in ( LrTable.NT 1, ( result, ID1left, expr2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (ID
 as ID1) = ID1 ()
 in (A.SimpleVar(sym(ID), IDleft))
end)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( _, DOTleft,
 _)) :: ( _, ( MlyValue.lvalue lvalue1, lvalue1left, _)) :: rest671))
 => let val  result = MlyValue.lvalue (fn _ => let val  (lvalue as 
lvalue1) = lvalue1 ()
 val  (ID as ID1) = ID1 ()
 in (A.FieldVar(lvalue, sym(ID), DOTleft))
end)
 in ( LrTable.NT 2, ( result, lvalue1left, ID1right), rest671)
end
|  ( 35, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: ( _, ( _, LBRACKleft, _)) :: ( _, ( MlyValue.ID ID1, (
IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.lvalue (fn _ => let val  (ID as ID1) = ID1 ()
 val  (expr as expr1) = expr1 ()
 in (
A.SubscriptVar(A.SimpleVar(sym(ID),
							   IDleft),
					       expr, LBRACKleft)
)
end)
 in ( LrTable.NT 2, ( result, ID1left, RBRACK1right), rest671)
end
|  ( 36, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: ( _, ( _, LBRACKleft, _)) :: ( _, ( MlyValue.lvalue lvalue1
, lvalue1left, _)) :: rest671)) => let val  result = MlyValue.lvalue
 (fn _ => let val  (lvalue as lvalue1) = lvalue1 ()
 val  (expr as expr1) = expr1 ()
 in (A.SubscriptVar(lvalue,
						   expr, LBRACKleft))
end)
 in ( LrTable.NT 2, ( result, lvalue1left, RBRACK1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.expr expr2, expr2left, expr2right)) :: _ ::
 ( _, ( MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  
result = MlyValue.exprSeq (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in ([(expr1, expr1left), (expr2, expr2left)])
end)
 in ( LrTable.NT 3, ( result, expr1left, expr2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.expr expr1, exprleft, expr1right)) :: _ :: 
( _, ( MlyValue.exprSeq exprSeq1, exprSeq1left, _)) :: rest671)) =>
 let val  result = MlyValue.exprSeq (fn _ => let val  (exprSeq as 
exprSeq1) = exprSeq1 ()
 val  (expr as expr1) = expr1 ()
 in (exprSeq@[(expr, exprleft)])
end)
 in ( LrTable.NT 3, ( result, exprSeq1left, expr1right), rest671)
end
|  ( 39, ( rest671)) => let val  result = MlyValue.funparam (fn _ => (
[]))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 40, ( ( _, ( MlyValue.expr expr1, expr1left, expr1right)) :: 
rest671)) => let val  result = MlyValue.funparam (fn _ => let val  (
expr as expr1) = expr1 ()
 in ([expr])
end)
 in ( LrTable.NT 4, ( result, expr1left, expr1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.funparam funparam1, funparam1left, _)) :: rest671)) => let
 val  result = MlyValue.funparam (fn _ => let val  (funparam as 
funparam1) = funparam1 ()
 val  (expr as expr1) = expr1 ()
 in (funparam@[expr])
end)
 in ( LrTable.NT 4, ( result, funparam1left, expr1right), rest671)
end
|  ( 42, ( ( _, ( _, _, RBRACE1right)) :: _ :: ( _, ( MlyValue.ID ID1,
 (IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.record (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.RecordExp({fields=[], typ=sym(ID),
				       pos=IDleft}))
end
)
 in ( LrTable.NT 5, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 43, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.recordFields
 recordFields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as 
ID1left), _)) :: rest671)) => let val  result = MlyValue.record (fn _
 => let val  (ID as ID1) = ID1 ()
 val  (recordFields as recordFields1) = recordFields1 ()
 in (
A.RecordExp({fields=recordFields,
						    typ=sym(ID),
						    pos=IDleft})
)
end)
 in ( LrTable.NT 5, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.recordFields (fn _ => let val  (ID as ID1) = ID1 ()
 val  (expr as expr1) = expr1 ()
 in ([(sym(ID), expr, IDleft)])
end)
 in ( LrTable.NT 6, ( result, ID1left, expr1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, COMMAleft, _)) :: ( _, ( 
MlyValue.recordFields recordFields1, recordFields1left, _)) :: rest671
)) => let val  result = MlyValue.recordFields (fn _ => let val  (
recordFields as recordFields1) = recordFields1 ()
 val  (ID as ID1) = ID1 ()
 val  (expr as expr1) = expr1 ()
 in (recordFields@[(sym(ID), expr, COMMAleft)])
end)
 in ( LrTable.NT 6, ( result, recordFields1left, expr1right), rest671)

end
|  ( 46, ( rest671)) => let val  result = MlyValue.decs (fn _ => ([]))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 47, ( ( _, ( MlyValue.tydecs tydecs1, _, tydecs1right)) :: ( _, (
 MlyValue.decs decs1, decs1left, _)) :: rest671)) => let val  result =
 MlyValue.decs (fn _ => let val  (decs as decs1) = decs1 ()
 val  (tydecs as tydecs1) = tydecs1 ()
 in (decs@[A.TypeDec(tydecs)])
end)
 in ( LrTable.NT 7, ( result, decs1left, tydecs1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.vardec vardec1, _, vardec1right)) :: ( _, (
 MlyValue.decs decs1, decs1left, _)) :: rest671)) => let val  result =
 MlyValue.decs (fn _ => let val  (decs as decs1) = decs1 ()
 val  (vardec as vardec1) = vardec1 ()
 in (decs@[vardec])
end)
 in ( LrTable.NT 7, ( result, decs1left, vardec1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.fundecs fundecs1, _, fundecs1right)) :: ( _
, ( MlyValue.decs decs1, decs1left, _)) :: rest671)) => let val  
result = MlyValue.decs (fn _ => let val  (decs as decs1) = decs1 ()
 val  (fundecs as fundecs1) = fundecs1 ()
 in (decs@[A.FunctionDec(fundecs)])
end)
 in ( LrTable.NT 7, ( result, decs1left, fundecs1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.tydec tydec1, tydec1left, tydec1right)) :: 
rest671)) => let val  result = MlyValue.tydecs (fn _ => let val  (
tydec as tydec1) = tydec1 ()
 in ([tydec])
end)
 in ( LrTable.NT 9, ( result, tydec1left, tydec1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.tydec tydec1, _, tydec1right)) :: ( _, ( 
MlyValue.tydecs tydecs1, tydecs1left, _)) :: rest671)) => let val  
result = MlyValue.tydecs (fn _ => let val  (tydecs as tydecs1) = 
tydecs1 ()
 val  (tydec as tydec1) = tydec1 ()
 in (tydecs@[tydec])
end)
 in ( LrTable.NT 9, ( result, tydecs1left, tydec1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (TYPEleft as TYPE1left), _)) :: 
rest671)) => let val  result = MlyValue.tydec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in ({name=sym(ID), ty=ty, pos=TYPEleft})
end)
 in ( LrTable.NT 8, ( result, TYPE1left, ty1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.NameTy(sym(ID), IDleft))
end)
 in ( LrTable.NT 10, ( result, ID1left, ID1right), rest671)
end
|  ( 54, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ty (fn _ => let val  (tyfields as tyfields1) =
 tyfields1 ()
 in (A.RecordTy(tyfields))
end)
 in ( LrTable.NT 10, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 55, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, (
ARRAYleft as ARRAY1left), _)) :: rest671)) => let val  result = 
MlyValue.ty (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.ArrayTy(sym(ID), ARRAYleft))
end)
 in ( LrTable.NT 10, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 56, ( rest671)) => let val  result = MlyValue.tyfields (fn _ => (
[]))
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 57, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.tyfields (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
[{name=sym(ID1),
		     escape=ref true,
		     typ=sym(ID2),
		     pos=ID1left}]
)
end)
 in ( LrTable.NT 11, ( result, ID1left, ID2right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, COMMAleft, _)) :: ( _, ( 
MlyValue.tyfields tyfields1, tyfields1left, _)) :: rest671)) => let
 val  result = MlyValue.tyfields (fn _ => let val  (tyfields as 
tyfields1) = tyfields1 ()
 val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
tyfields@[{name=sym(ID1),
					     escape=ref true,
					     typ=sym(ID2),
					     pos=COMMAleft}]
)
end)
 in ( LrTable.NT 11, ( result, tyfields1left, ID2right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.fundec fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.fundecs (fn _ => let val 
 (fundec as fundec1) = fundec1 ()
 in ([fundec])
end)
 in ( LrTable.NT 14, ( result, fundec1left, fundec1right), rest671)

end
|  ( 60, ( ( _, ( MlyValue.fundec fundec1, _, fundec1right)) :: ( _, (
 MlyValue.fundecs fundecs1, fundecs1left, _)) :: rest671)) => let val 
 result = MlyValue.fundecs (fn _ => let val  (fundecs as fundecs1) = 
fundecs1 ()
 val  (fundec as fundec1) = fundec1 ()
 in (fundecs@[fundec])
end)
 in ( LrTable.NT 14, ( result, fundecs1left, fundec1right), rest671)

end
|  ( 61, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: _ :: (
 _, ( MlyValue.tyfields tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID 
ID1, _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left), _)) :: 
rest671)) => let val  result = MlyValue.fundec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  (expr as expr1) = expr1 ()
 in (
{name=sym(ID),
						      params=tyfields,
						      result=NONE,
						      body=expr,
						      pos=FUNCTIONleft}
)
end)
 in ( LrTable.NT 13, ( result, FUNCTION1left, expr1right), rest671)

end
|  ( 62, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _,
 (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result
 = MlyValue.fundec (fn _ => let val  ID1 = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  ID2 = ID2 ()
 val  (expr as expr1) = expr1 ()
 in (

	     {name=sym(ID1),
	      params=tyfields,
	      result=SOME((sym(ID2), ID2left)),
	      body=expr, pos=FUNCTIONleft}
)
end)
 in ( LrTable.NT 13, ( result, FUNCTION1left, expr1right), rest671)

end
|  ( 63, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (VARleft as VAR1left), _)) :: 
rest671)) => let val  result = MlyValue.vardec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (expr as expr1) = expr1 ()
 in (
A.VarDec({name=sym(ID), escape=ref true,
				       typ=NONE, init=expr,
				       pos=VARleft})
)
end)
 in ( LrTable.NT 12, ( result, VAR1left, expr1right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: ( _, ( _, COLONleft, _)) :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (VARleft as VAR1left), _)) :: 
rest671)) => let val  result = MlyValue.vardec (fn _ => let val  ID1 =
 ID1 ()
 val  ID2 = ID2 ()
 val  (expr as expr1) = expr1 ()
 in (
A.VarDec({name=sym(ID1),
						escape=ref true,
						typ=SOME((sym(ID2),
							  COLONleft)),
						init=expr,
						pos=VARleft})
)
end)
 in ( LrTable.NT 12, ( result, VAR1left, expr1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
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
fun LOWERTHANOPHACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
end
end
