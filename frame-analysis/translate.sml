structure Translate =
struct

(* TODO: this is going to be changed? *)
type exp = unit

(* a level contains a frame and a parent frame *)
type level = {frame: Frame.frame, parent: Frame.frame}

(* Note this is different than Frame.access, also includes a level *)
type access = level * Frame.access

(* the outermost frame is the parent frame of tiger programs and the frame
   in which the library functions are declared *)
val outermostFrame = Frame.newFrame({name=Temp.newLabel(), formals=[]})

(* The outermost level *)
val outermost = {frame=outermostFrame, parent=outermostFrame}

(* Creates a new level with the given parent level, a name, and a list of booleans
   corresponding to the escapes for the formal parameters *)
fun newLevel({parent:level, name: Temp.label, formals:bool list}) =
    (* add on true to the formals list to represent the static link in the frame *)
    {frame=Frame.newFrame({name=name, formals=true::formals}),
     parent=(#frame parent)}

(* Return a list of Translate.accesses associated with the formal parameters of the given level *)
(* Removes the first formal parameter returned from Frame since this will correspond to the
   static link *)
fun formals(level:level) = map (fn (a) => (level, a)) (tl(Frame.formals(#frame(level))))

(* Return a function that takes a boolean and returns the appropriate access for
   the new local variable *)
fun allocLocal(level:level) = (fn (b) => (level, Frame.allocLocal(#frame level)(b)))

end
