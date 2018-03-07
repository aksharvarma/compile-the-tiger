structure Translate =
struct

(* TODO: this is going to be changed? *)
type exp = unit

(* Basic changes to Translate based on IR chapter *)
(* datatype exp = Ex of Tree.exp *)
(* 	     | Nx of Tree.stm *)
(* 	     | Cx of Temp.label * Temp.label -> Tree.stm *)
(* Signatures for un_x funcstions, and empty definitions *)
(* val unEx: Translate.exp -> exp *)
(* val unNx: Translate.exp -> stm *)
(* val unCx: Translate.exp -> (label * label -> stm) *)	    
(* fun unEx(e) = CONST(0) *)
(* fun unNx(e) = EXP(CONST(0)) *)
(* fun unCx(e) = (fn (l1, l2) => EXP(CONST(0))) *)
	     
	     
(* a level contains a frame and a parent level.
   the outermost level is a special level (OUTERMOST) with no frame or parent *)
datatype level = OUTERMOST | Lev of Frame.frame * level

(* Note this is different than Frame.access, also includes a level *)
type access = level * Frame.access

(* thrown if allocLocal is called on the outermost level *)
exception OutermostException

(* the outermost level *)
val outermost = OUTERMOST

(* Creates a new level with the given parent level, a name, and a list of booleans
   corresponding to the escapes for the formal parameters *)
fun newLevel({parent:level, name: Temp.label, formals:bool list}) =
    (* add on true to the formals list to represent the static link in the frame *)
    Lev(Frame.newFrame({name=name, formals=true::formals}), parent)

(* Return a list of Translate.accesses associated with the formal parameters of the given level *)
(* Removes the first formal parameter returned from Frame since this will correspond to the
   static link *)
fun formals(Lev(f,p)) = map (fn (a) => (Lev(f,p), a)) (tl(Frame.formals(f)))
  | formals(OUTERMOST) = []

(* Return a function that takes a boolean and returns the appropriate access for
   the new local variable *)
fun allocLocal(Lev(f,p)) = (fn (b) => (Lev(f,p), Frame.allocLocal(f)(b)))
  (* this should never occur, as the only things defined in the outermost level
     are the library functions *)
  | allocLocal(OUTERMOST) = raise OutermostException

end
