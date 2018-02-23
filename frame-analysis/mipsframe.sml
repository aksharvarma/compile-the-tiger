structure MipsFrame :> FRAME =
struct

val wordSize = 4

datatype access = InReg of Temp.temp | InFrame of int

type frame = {name:Temp.label,
	      formals: access list,
              locals: int ref}

(* TODO:  write view shift instructions *)
fun newFrame({name: Temp.label, formals: bool list})=
    let
        fun createAccesses([], index) = []
          | createAccesses(f::fs, index) =
                (if f then InFrame(index * wordSize) else InReg(Temp.newTemp()))
                ::createAccesses(fs, index + 1)
    in
        {name=name,
        formals= createAccesses(formals, 0),
        locals= ref 0}
    end

fun name({name, formals, locals}) = name

fun formals({name, formals, locals}) = formals

fun allocLocal({name, formals, locals}) =
    (locals := !locals + 1;
    fn (b) => if b then InFrame(~(!locals) * wordSize) else InReg(Temp.newTemp()))

end

structure Frame :> FRAME = MipsFrame
