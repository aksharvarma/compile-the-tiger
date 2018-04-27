(* Automates task of compiling all files inside a folder. *)
structure CompileAll =
struct

(* compileAllWithRuntime: string * string -> unit
 *
 * Compile all files inside the directory using the given runtime
 * Creates ".s" assembly code as well as ".s.exec" which has runtime appended
 *)
fun compileAllWithRuntime(directory, runtime) =
    let
      fun getFileList() =
          let
            val dir = OS.FileSys.openDir(directory)
            fun loop(l) =
                case OS.FileSys.readDir(dir)
                 of SOME(f:string) =>
                    if not((String.isSuffix ".s" f)
                           orelse (String.isSuffix ".swor" f))
                    then loop((directory^f)::l) else loop(l)
                  | NONE => l
          in
            loop([])
          end

      val compileFn = (fn filename =>
                          Main.compileToExecutable(runtime, filename))
      val files = getFileList()

    in (app (fn f => compileFn(f)) files) end

(* compileAll: string -> unit
 *
 * Compile all files inside directory using "runtime.s"
 * Creates ".s" assembly code as well as ".s.exec" which has runtime appended
 *)
fun compileAll(directory) =
    let val ourRuntime = "runtime.s"
    in compileAllWithRuntime(directory, ourRuntime) end
end
