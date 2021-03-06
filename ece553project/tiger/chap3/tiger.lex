type svalue = Tokens.svalue
type pos = int
  type ('a, 'b) token = ('a, 'b) Tokens.token
  type lexresult =(svalue,pos) token


val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commentCounter = ref 0
val insideString = ref 0
val currString : string ref = ref ""
val startString = ref 0

  
fun eof() = let val pos = hd(!linePos) in

  if(!commentCounter > 0) then ErrorMsg.error pos ("Error: There is an unclosed comment at the end of the file")

    else if(!insideString=1) then ErrorMsg.error pos("Error: There is an unclosed string at the end of the file")
    else ();
    
    
   
    Tokens.EOF(pos,pos) end




%%
   %header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
  %s COMMENT STRING;
whitespace=(" "|\t|\n|\f);
%%


<INITIAL>"/*" => (YYBEGIN COMMENT; commentCounter:=1; continue());
<COMMENT>"/*" => (commentCounter := (!commentCounter+1); continue());
<COMMENT>[\n]+	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT>[\t]+ => (continue ());
<COMMENT>"*/" => (commentCounter := (!commentCounter-1);
		  if(!commentCounter=0) then (YYBEGIN INITIAL)
		    else();
		  continue());
		
<COMMENT>. => (continue());

<INITIAL>"\"" => (YYBEGIN STRING;
		   startString := yypos;
		   currString := "";
                   insideString :=1; 
		  continue());
<STRING>"\"" => (YYBEGIN INITIAL; insideString :=0;
		 Tokens.STRING(!currString, !startString, !startString+size(!currString)));
<STRING>\\t => (currString := !currString ^ "\t"; continue());
<STRING>\\n => (currString := !currString ^ "\n"; continue());
<STRING>\^[a-zA-Z] => (currString := !currString ^ yytext; continue());
<STRING>\ddd =>  (currString := !currString ^ yytext; continue());
<STRING>\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<STRING>\r	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<STRING>(\t)+ => (continue());
<STRING>"\\\"" => (currString := !currString ^ "\""; continue());
<STRING>\\\\ => (currString := !currString ^"\\"; continue());
<STRING>\\{whitespace}+\\ => (continue());
<STRING>\\. => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
<STRING>.   => (currString := !currString ^yytext; continue());
	

<INITIAL>\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>\r	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL>type     => (Tokens.TYPE(yypos, yypos+4));
<INITIAL>function => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL>break    => (Tokens.BREAK(yypos, yypos+5));
<INITIAL>of       => (Tokens.OF(yypos, yypos+2));
<INITIAL>end      => (Tokens.END(yypos, yypos+3));
<INITIAL>in       => (Tokens.IN(yypos, yypos+2));
<INITIAL>nil      => (Tokens.NIL(yypos, yypos+3));
<INITIAL>let      => (Tokens.LET(yypos, yypos+3));
<INITIAL>do       => (Tokens.DO(yypos, yypos+2));
<INITIAL>to       => (Tokens.TO(yypos, yypos+2));
<INITIAL>for      => (Tokens.FOR(yypos, yypos+3));
<INITIAL>while    => (Tokens.WHILE(yypos, yypos+5));
<INITIAL>else     => (Tokens.ELSE(yypos, yypos+4));
<INITIAL>then     => (Tokens.THEN(yypos, yypos+4));
<INITIAL>if       => (Tokens.IF(yypos, yypos+2));
<INITIAL>array    => (Tokens.ARRAY(yypos, yypos +5));
<INITIAL>:=       => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL>"|"        => (Tokens.OR(yypos, yypos+1));
<INITIAL>"&"        => (Tokens.AND(yypos, yypos+1));
<INITIAL>">="       => (Tokens.GE(yypos, yypos+2));
<INITIAL>">"        => (Tokens.GT(yypos, yypos+1));
<INITIAL>"<="       => (Tokens.LE(yypos, yypos+2));
<INITIAL>"<"        => (Tokens.LT(yypos, yypos+1)); 
<INITIAL>"<>"       => (Tokens.NEQ(yypos, yypos+2));
<INITIAL>"="        => (Tokens.EQ(yypos, yypos+1));
<INITIAL>"/"        => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL>"*"        => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"-"        => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"+"        => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"."        => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"}"        => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"{"        => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"]"        => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"["        => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>")"        => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"("        => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>";"        => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>":"        => (Tokens.COLON(yypos,yypos+1));
<INITIAL>","        => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>" " => (continue());
<INITIAL>\t  => (continue());
<INITIAL>[0-9]*     => (Tokens.INT(valOf(Int.fromString (yytext)), yypos,yypos+size(yytext)));
<INITIAL>[a-zA-Z][a-zA-Z0-9_]*  => (Tokens.ID(yytext, yypos, yypos+ size(yytext)));

<INITIAL>.          => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());


