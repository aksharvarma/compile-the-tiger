(* ML Declarations *)
type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

val commentNesting = ref 0
val inString = ref false
val matchedString = ref ""
fun eof() =
  let
  val pos = hd(!linePos)
  in
  if !commentNesting <> 0 then
  ErrorMsg.error yypos ("Unclosed comment::" ^ yytext)
  else if !inString then
  ErrorMsg.error yypos ("Unclosed string literal::" ^ yytext)
    else ()
	 Tokens.EOF(yypos,yypos)
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

%s COMMENT;
%s STRING;

%%
\n       => (linenum := !linenum+1; continue());

"type"   => (Tokens.TYPE(yypos,yypos+4));
"var"   => (Tokens.VAR(yypos,yypos+3));
"function"   => (Tokens.FUNCTION(yypos,yypos+8));
"break"   => (Tokens.BREAK(yypos,yypos+5));
"of"   => (Tokens.OF(yypos,yypos+2));
"end"   => (Tokens.END(yypos,yypos+3));
"in"   => (Tokens.IN(yypos,yypos+2));
"nil"   => (Tokens.NIL(yypos,yypos+3));
"let"   => (Tokens.LET(yypos,yypos+3));
"do"   => (Tokens.DO(yypos,yypos+2));
"to"   => (Tokens.TO(yypos,yypos+2));
"for"   => (Tokens.FOR(yypos,yypos+3));
"while"  => (Tokens.WHILE(yypos,yypos+5));
"else"   => (Tokens.ELSE(yypos,yypos+4));
"if"     => (Tokens.IF(yypos,yypos+2));
"then"   => (Tokens.THEN(yypos,yypos+4));
"array"  => (Tokens.ARRAY(yypos,yypos+5));


","      => (Tokens.COMMA(yypos,yypos+1));
":"      => (Tokens.COLON(yypos,yypos+1));
";"      => (Tokens.SEMICOLON(yypos,yypos+1));
"("      => (Tokens.LPAREN(yypos,yypos+1));
")"      => (Tokens.RPAREN(yypos,yypos+1));
"["      => (Tokens.LBRACK(yypos,yypos+1));
"]"      => (Tokens.RBRACK(yypos,yypos+1));
"{"      => (Tokens.LBRACE(yypos,yypos+1));
"}"      => (Tokens.RBRACE(yypos,yypos+1));
"."      => (Tokens.DOT(yypos,yypos+1));
"+"      => (Tokens.PLUS(yypos,yypos+1));
"-"      => (Tokens.MINUS(yypos,yypos+1));
"*"      => (Tokens.TIMES(yypos,yypos+1));
"/"      => (Tokens.DIVIDE(yypos,yypos+1));
"="      => (Tokens.EQ(yypos,yypos+1));
"<>"     => (Tokens.NEQ(yypos,yypos+2));
"<"      => (Tokens.LT(yypos,yypos+1));
">"      => (Tokens.GT(yypos,yypos+1));
"<="     => (Tokens.LE(yypos,yypos+2));
">="     => (Tokens.GE(yypos,yypos+2));
"|"      => (Tokens.OR(yypos,yypos+1));
"&"      => (Tokens.AND(yypos,yypos+1));
":="     => (Tokens.ASSIGN(yypos,yypos+2));


{digit}+ => (Tokens.INT(Int.fromString(yytext), yypos, yypos+size(yytext));


{alpha}({alpha}|{digit}|"_")* => (Tokens.ID(yytext, yypos, yypos+size(yytext));


"(*"     => (commentNesting := !commentNesting+1; YYBEGIN COMMENT; continue());
<COMMENT>"*)" => (commentNesting := !commentNesting-1; if !commentNesting = 0 then YYBEGIN INITIAL; continue());
<COMMENT>. => (continue());

				  
\"       => (inString := true; stringStart := yypos+1; YYBEGIN STRING; continue());
<STRING>"\n" => (matchedString := !matchedString ^ "\n"; continue());
<STRING>"\t" => (matchedString := !matchedString ^ "\t"; continue());
<STRING>"\"" => (matchedString := !matchedString ^ "\""; continue());
<STRING>"\\" => (matchedString := !matchedString ^ "\\"; continue());
<STRING>\ddd => (matchedString := !matchedString ^ chr(yytext)); continue());
<STRING>"\\"({ws}|\n)+"\\" => (continue());
<STRING>"\\^"[@A-Z\\[]^_.] => (matchedString := !matchedString ^ yytext; continue());
<STRING>[^\"]* => (matchedString := !matchedString ^ yytext; continue());
<STRING>\"   => (inString := false; Token.STRING(matchedString, stringStart, yypos)YYBEGIN INITIAL; continue());
				  

{ws}+    => (continue());

.        => (ErrorMsg.error yypos ("illegal character" ^ yytext); continue());

