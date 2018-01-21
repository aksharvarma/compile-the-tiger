signature ERRORMSG =
sig
    val anyErrors : bool ref
    val fileName : string ref
    val lineNum : int ref
    val linePos : int list ref
    val errorList : (int * int * string) list ref
    val sourceStream : TextIO.instream ref
    val error : (int * int * string) -> unit
    exception myError
    val impossible : string -> 'a   (* raises Error *)
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

exception myError

fun error (posStart:int, posEnd:int, msg:string) =
    let
	fun mylook (_,0,n)  = print(Int.toString(n)^".0")
	  | mylook(pos::posList, searchpos, n) =
	    if pos<searchpos then app print [Int.toString n,
					     ".",
					     Int.toString(searchpos-pos-1)]
	    else mylook (posList, searchpos, n-1)
	  | mylook (_)  = print("00")
    in anyErrors := true;
       print((!fileName)^":");
       print("[");
       mylook(!linePos, posStart, !lineNum);
       print("--");
       if posEnd = 0 then mylook(!linePos, posEnd, !lineNum+1)
       else mylook(!linePos, posEnd, !lineNum);
       print("] ");
       print "Error: ";
       print msg;
       print "\n"
    end

fun impossible(msg) =
    (app print ["Error: Compiler bug: ",msg,"\n"];
     TextIO.flushOut TextIO.stdOut;
     raise myError)

end  (* structure ErrorMsg *)
