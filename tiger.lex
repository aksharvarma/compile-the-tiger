(* ML Declarations *)
type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val errorList = ErrorMsg.errorList
  (*fun err(p1,p2) = ErrorMsg.error p1*)

val commentNesting = ref 0
val inString = ref false
val matchedString = ref ""
val stringStart = ref 0

fun loopThroughErrorList() =
  let
  val eL = rev(!errorList)

  fun  printList([]) = ()
  | printList(x::xs) = (let val (sLinNum,sColOff,eLinNum,eColOff,msg) = x
			in print(Int.toString(sLinNum)^"."^Int.toString(sColOff)^"-"^Int.toString(eLinNum)^"."^Int.toString(eColOff)^"Error: "^msg) end ;
			printList(xs))
  in printList(eL)
  end
  
fun eof() =
  let
  val pos = hd(!linePos)
  val finalCommentNesting:int = !commentNesting
  in
  if !commentNesting <> 0 then
  (commentNesting := 0; ErrorMsg.error pos ("Unclosed comment at EOF"^Int.toString(finalCommentNesting)))
  else if !inString then
  ErrorMsg.error pos ("Unclosed string literal at EOF")
    else loopThroughErrorList();
  Tokens.EOF(pos,pos)
  end

(* Lex definitions *)
(* we assume there are no \r being used in {ws}.*)
(* COMMENT and STRING states defined here *)
%%
alpha=[A-Za-z];
Upperalpha=[A-Z];
Loweralpha=[a-z];
digit=[0-9];
ws = [ \t];
escape = [\]@A-Z\\_.?\[^];
notEscape = [^\]@A-Z\\_.?\[^];
%s COMMENT STRING;

%%
<INITIAL, COMMENT>\n|\r       => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<INITIAL>"type"   => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>"var"   => (Tokens.VAR(yypos,yypos+3));
<INITIAL>"function"   => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>"break"   => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>"of"   => (Tokens.OF(yypos,yypos+2));
<INITIAL>"end"   => (Tokens.END(yypos,yypos+3));
<INITIAL>"in"   => (Tokens.IN(yypos,yypos+2));
<INITIAL>"nil"   => (Tokens.NIL(yypos,yypos+3));
<INITIAL>"let"   => (Tokens.LET(yypos,yypos+3));
<INITIAL>"do"   => (Tokens.DO(yypos,yypos+2));
<INITIAL>"to"   => (Tokens.TO(yypos,yypos+2));
<INITIAL>"for"   => (Tokens.FOR(yypos,yypos+3));
<INITIAL>"while"  => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>"else"   => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>"if"     => (Tokens.IF(yypos,yypos+2));
<INITIAL>"then"   => (Tokens.THEN(yypos,yypos+4));
<INITIAL>"array"  => (Tokens.ARRAY(yypos,yypos+5));



<INITIAL>","      => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>":"      => (Tokens.COLON(yypos,yypos+1));
<INITIAL>";"      => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"("      => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"      => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"["      => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"      => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"{"      => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"      => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"."      => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"+"      => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-"      => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"*"      => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"/"      => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"="      => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"<>"     => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"<"      => (Tokens.LT(yypos,yypos+1));
<INITIAL>">"      => (Tokens.GT(yypos,yypos+1));
<INITIAL>"<="     => (Tokens.LE(yypos,yypos+2));
<INITIAL>">="     => (Tokens.GE(yypos,yypos+2));
<INITIAL>"|"      => (Tokens.OR(yypos,yypos+1));
<INITIAL>"&"      => (Tokens.AND(yypos,yypos+1));
<INITIAL>":="     => (Tokens.ASSIGN(yypos,yypos+2));


<INITIAL>{digit}+ => (Tokens.INT(valOf(Int.fromString(yytext)),
			yypos, yypos+size(yytext)));


<INITIAL>{alpha}({alpha}|{digit}|"_")* => (Tokens.ID(yytext,
					    yypos, yypos+size(yytext)));


<INITIAL, COMMENT>"/*"     => (commentNesting := !commentNesting+1;
			       YYBEGIN COMMENT; continue());
<COMMENT>"*/" => (commentNesting := !commentNesting-1;
		  if !commentNesting = 0
		  then YYBEGIN INITIAL else YYBEGIN COMMENT;
		  continue());
<COMMENT>. => (continue());



<INITIAL>\"       => (inString := true; stringStart := yypos+1; matchedString := "";
		      YYBEGIN STRING; continue());
<STRING>"H" => (errorList := (!lineNum, yypos - hd(!linePos),
			      !lineNum, yypos - hd(!linePos)+size(yytext),
			      "caught a hyphen: illegal escape character:" ^ yytext) :: !errorList;
		continue());
<STRING>"\^"{notEscape} => (errorList := (!lineNum, yypos - hd(!linePos),
			      !lineNum, yypos - hd(!linePos)+size(yytext),
			      "caught a hyphen: illegal escape character:" ^ yytext) :: !errorList;
		continue());
<STRING>\\[n\\t\"\ddd] => (matchedString := !matchedString ^ yytext;
			   if ((yytext = "\n") orelse (yytext = "\r")) then (lineNum := !lineNum+1; linePos := yypos :: !linePos) else ();
		 continue());
<STRING>"\"({ws}|\n)+"\" => (continue());
<STRING>[^"]* => (matchedString := !matchedString ^ yytext;
		   continue());
<STRING>\"   => (inString := false; YYBEGIN INITIAL;
		 Tokens.STRING(!matchedString, !stringStart, yypos));


<INITIAL>{ws}    => (continue());

<INITIAL>.        => (errorList := (!lineNum, yypos - hd(!linePos),
			      !lineNum, yypos - hd(!linePos)+size(yytext),
			      "Illegal character error:" ^ yytext) :: !errorList;
continue());



