structure MipsFrame : FRAME = 
struct

datatype access = InReg of Temp.temp | InFrame of int

type frame = {name:Temp.label,
	      formals: (bool * access) list,
	      locals: int}

						    
fun newFrame({name: Temp.label, formals: bool list})=
    {name=name,
     formals=(map (fn (b:bool) => (b, InFrame(0))) formals),
     locals=0}

fun name({name, formals, locals}) = name
				      
fun formals({name, formals:(bool*access) list, locals}) =
    (map #2 formals)

fun allocLocal({name, formals, locals}) =  (fn b => InFrame(0))

end

structure Frame:FRAME = MipsFrame
