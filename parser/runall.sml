fun test(i) = "test"^Int.toString(i)^".tig"

fun runall() =
    let
      val dir = "../ignore-tiger/tiger/testcases/"
      val i = ref 49
      val asts = ref []
    in
      while !i > 0 do
		   (print(dir^test(!i)^"\n");
		    asts := ((!i, Parse.parse(dir^test(!i))) :: !asts);
		    i := !i-1);
      !asts
    end

fun printASTlist([]) = ()
  | printASTlist(((i, ast)::xs)) = (print("\nFILE::"^Int.toString(i)
				       ^":\n");
				 PrintAbsyn.print(TextIO.stdOut, ast);
				 printASTlist(xs))
