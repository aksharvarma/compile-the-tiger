signature ERRORMSG =
sig
    val anyErrors : bool ref
    val fileName : string ref
    val lineNum : int ref
    val linePos : int list ref
    val errorList : (int * int * string) list ref
    val sourceStream : TextIO.instream ref
    val error : (int * int * string) -> unit
    exception Error
    val throwError : unit -> 'a   (* raises CompilerError *)
    val reset : unit -> unit
end

structure ErrorMsg : ERRORMSG =
struct

val anyErrors = ref false
val fileName = ref ""
val lineNum = ref 1
val linePos = ref [0]
val sourceStream = ref TextIO.stdIn

val errorList: (int * int * string) list ref = ref []

fun reset() = (anyErrors:=false;
	       fileName:="";
	       lineNum:=1;
	       linePos:=[0];
	       sourceStream:=TextIO.stdIn;
	       errorList:= [])

exception Error

(* Prints an error in the format "fileName:[x.y--w.z] Error: msg" *)
fun error (posStart:int, posEnd:int, msg:string) =
    let
        (* Given a position corresponding to the end of a line,
           a position from the start of the file,
           and a line number,
           prints a string in the format x.y where
             - x is the line number of the position
             - y is the position's column offset from the start
               of the line
        *)
        fun mylook (_, 0, n)  = print(Int.toString(n) ^ ".0")
          | mylook(pos::posList, searchpos, n) =
	        if pos < searchpos
                then app print [Int.toString n, ".", Int.toString(searchpos-pos)]
	        else mylook (posList, searchpos, n-1)
	  | mylook ([], searchpos, n) = print("0.0");
    in
        anyErrors := true;
        (* print(Int.toString(!lineNum)); *)
        print((!fileName)^": ");
        print("[");
        mylook(!linePos, posStart, !lineNum);
        print("--");
        if posEnd = 0
        then mylook(!linePos, posEnd, !lineNum+1)
        else mylook(!linePos, posEnd, !lineNum);
        print("] ");
        print "Error: ";
        print msg;
        print "\n"
    end

fun throwError() =
    (app print ["\nError: Compiler encountered ",
        Int.toString(length(!errorList)),
        " errors\n"];
    reset();
    TextIO.flushOut TextIO.stdOut;
    raise Error)

end  (* structure ErrorMsg *)
