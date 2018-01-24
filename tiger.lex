(* ML Declarations *)
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val errorList = ErrorMsg.errorList

val commentNesting = ref 0
val inString = ref false
val matchedString = ref []: string list ref
val stringStart = ref 0

fun eof(fileName) =
    let
        val pos = hd(!linePos)
	val finalCommentNesting:int = !commentNesting
    in
	(* If inside a comment at EOF, add to errorList *)
	if !commentNesting <> 0
        then (commentNesting := 0;
	    errorList:= (0, 0, "Open comment at EOF. Nesting level: " ^
                Int.toString(finalCommentNesting)) :: !errorList)
	else if !inString
        then (* If inside a string at EOF, add to errorList *)
	    (inString := false;
            errorList:= (0, 0, "Unclosed string at EOF.") :: !errorList)
	else ();

        (* Set the filename for error printing *)
        ErrorMsg.fileName := fileName;
	(* If there were errors, print them and throw an exception *)
        if (length(!errorList) > 0)
        then (app ErrorMsg.error (rev (!errorList));
            ErrorMsg.throwCompilerError())
        else ();

	(* Resets errors lineNum, etc. *)
	ErrorMsg.reset();

	(* The EOF token position might appear to be slightly off due to
	   presence/lack of a newline at the end of the file*)
	Tokens.EOF(pos,pos)
    end

(* Lex definitions *)
(* COMMENT, STRING, and SKIPSTRING states defined here *)
%%

alpha=[A-Za-z];
digit=[0-9];
ws = [ \t];
escape = [n\\t\"\ddd];
controlEscape = [\]@A-Z\\_\[^];
notEscape = [^n\\t\"\ddd];
notControlEscape = [^\]@A-Z\\_\[^];

%s COMMENT STRING SKIPSTRING;
%arg (fileName:string);

%%
<INITIAL, COMMENT, SKIPSTRING>\n|\r   => (
		       (* Count lines and store line-end position *)
		       lineNum := !lineNum+1;
		       linePos := yypos :: !linePos;
		       continue());

<INITIAL>"type"     => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>"var"      => (Tokens.VAR(yypos,yypos+3));
<INITIAL>"function" => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>"break"    => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>"of"       => (Tokens.OF(yypos,yypos+2));
<INITIAL>"end"      => (Tokens.END(yypos,yypos+3));
<INITIAL>"in"       => (Tokens.IN(yypos,yypos+2));
<INITIAL>"nil"      => (Tokens.NIL(yypos,yypos+3));
<INITIAL>"let"      => (Tokens.LET(yypos,yypos+3));
<INITIAL>"do"       => (Tokens.DO(yypos,yypos+2));
<INITIAL>"to"       => (Tokens.TO(yypos,yypos+2));
<INITIAL>"for"      => (Tokens.FOR(yypos,yypos+3));
<INITIAL>"while"    => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>"else"     => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>"if"       => (Tokens.IF(yypos,yypos+2));
<INITIAL>"then"     => (Tokens.THEN(yypos,yypos+4));
<INITIAL>"array"    => (Tokens.ARRAY(yypos,yypos+5));

<INITIAL>","        => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>":"        => (Tokens.COLON(yypos,yypos+1));
<INITIAL>";"        => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"("        => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"        => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"["        => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"        => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"{"        => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"        => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"."        => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"+"        => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-"        => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"*"        => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"/"        => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"="        => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"<>"       => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"<"        => (Tokens.LT(yypos,yypos+1));
<INITIAL>">"        => (Tokens.GT(yypos,yypos+1));
<INITIAL>"<="       => (Tokens.LE(yypos,yypos+2));
<INITIAL>">="       => (Tokens.GE(yypos,yypos+2));
<INITIAL>"|"        => (Tokens.OR(yypos,yypos+1));
<INITIAL>"&"        => (Tokens.AND(yypos,yypos+1));
<INITIAL>":="       => (Tokens.ASSIGN(yypos,yypos+2));

<INITIAL>{digit}+ =>
    ((* Integers *)
    Tokens.INT(valOf(Int.fromString(yytext)), yypos, yypos + size(yytext)));

<INITIAL>{alpha}({alpha}|{digit}|"_")* =>
    ((* Identifiers *)
    Tokens.ID(yytext, yypos, yypos+size(yytext)));

<INITIAL, COMMENT>"/*"  =>
    ((* Comment starts, increment nesting *)
    commentNesting := !commentNesting + 1;
    YYBEGIN COMMENT;
    continue());
<COMMENT>"*/" =>
    ((* Comment ends, change state based on nesting *)
    commentNesting := !commentNesting - 1;
    if !commentNesting = 0
    then YYBEGIN INITIAL
    else YYBEGIN COMMENT;
    continue());
<COMMENT>. =>
    ((* In the middle of the comment, continue *)
    continue());

<INITIAL>\" =>
    ((* String literal starts *)
    inString := true;
    stringStart := yypos+1;
    matchedString := [];
    YYBEGIN STRING;
    continue());

<STRING>[^\032-\127] =>
    ((* Catch non-printable characters *)
    errorList := (yypos, yypos+size(yytext),
	"Illegal non-printing character in string:" ^ yytext) :: !errorList;
    continue());

<STRING>\\({ws}|\n|\r) =>
    ((* Skip whitespace between two backslashes
	Uses a special state called SKIPSTRING *)
    if (yytext = "\\\n" orelse yytext = "\\\r")
    then (lineNum := !lineNum+1; linePos := yypos :: !linePos)
    else ();
    YYBEGIN SKIPSTRING;
    continue());

<SKIPSTRING>{ws}+ => (continue());

<SKIPSTRING>\\   => (YYBEGIN STRING; continue());

<STRING>(\\{notEscape}|\\"^"{notControlEscape}) =>
    ((* Catch all illegal escapes *)
    errorList := (yypos, yypos +size(yytext),
        "Illegal escape character: " ^ yytext) :: !errorList;
    continue());

<STRING>[^"]|\\\\ =>
    ((* Double backslash is addressed here because
	that regex was easier to write *)
    matchedString := yytext :: !matchedString;
    continue());
<STRING>\" =>
    ((* String literal ends, token is generated here. *)
    inString := false;
    YYBEGIN INITIAL;
    Tokens.STRING(concat(rev(!matchedString)), !stringStart, yypos));

<INITIAL>{ws} =>  ((* Skip whitespace *) continue());

<INITIAL>. =>
    ((* Catch any other illegal characters *)
    errorList := (yypos, yypos+size(yytext),
        "Illegal character error:"^yytext) :: !errorList;
    continue());

