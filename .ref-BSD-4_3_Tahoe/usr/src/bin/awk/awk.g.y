/*	awk.g.y	4.2	87/09/16	*/

%token	FIRSTTOKEN	/*must be first*/
%token	FINAL FATAL
%token	LT LE GT GE EQ NE
%token	MATCH NOTMATCH
%token	APPEND
%token	ADD MINUS MULT DIVIDE MOD UMINUS 
%token	ASSIGN ADDEQ SUBEQ MULTEQ DIVEQ MODEQ
%token	JUMP
%token	XBEGIN XEND
%token	NL
%token	PRINT PRINTF SPRINTF SPLIT
%token	IF ELSE WHILE FOR IN NEXT EXIT BREAK CONTINUE
%token	PROGRAM PASTAT PASTAT2

%right	ASGNOP
%left	BOR
%left	AND
%left	NOT
%left	NUMBER VAR ARRAY FNCN SUBSTR LSUBSTR INDEX
%left	GETLINE
%nonassoc RELOP MATCHOP
%left	OR
%left	STRING  DOT CCL NCCL CHAR
%left	'(' '^' '$'
%left	CAT
%left	'+' '-'
%left	'*' '/' '%'
%left	STAR PLUS QUEST
%left	POSTINCR PREINCR POSTDECR PREDECR INCR DECR
%left	FIELD INDIRECT
%token	LASTTOKEN	/* has to be last */

%{
#include "awk.def"
#ifndef	DEBUG	
#	define	PUTS(x)
#endif
%}
%%

program:
	  begin pa_stats end	{ if (errorflag==0) winner = (node *)stat3(PROGRAM, $1, $2, $3); }
	| error			{ yyclearin; yyerror("bailing out"); }
	;

begin:
	  XBEGIN '{' stat_list '}'	{ PUTS("XBEGIN list"); $$ = $3; }
	| begin NL
	| 	{ PUTS("empty XBEGIN"); $$ = (hack)nullstat; }
	;

end:
	  XEND '{' stat_list '}'	{ PUTS("XEND list"); $$ = $3; }
	| end NL
	|	{ PUTS("empty END"); $$ = (hack)nullstat; }
	;

compound_conditional:
	  conditional BOR conditional	{ PUTS("cond||cond"); $$ = op2(BOR, $1, $3); }
	| conditional AND conditional	{ PUTS("cond&&cond"); $$ = op2(AND, $1, $3); }
	| NOT conditional		{ PUTS("!cond"); $$ = op1(NOT, $2); }
	| '(' compound_conditional ')'	{ $$ = $2; }
	;

compound_pattern:
	  pattern BOR pattern	{ PUTS("pat||pat"); $$ = op2(BOR, $1, $3); }
	| pattern AND pattern	{ PUTS("pat&&pat"); $$ = op2(AND, $1, $3); }
	| NOT pattern		{ PUTS("!pat"); $$ = op1(NOT, $2); }
	| '(' compound_pattern ')'	{ $$ = $2; }
	;

conditional:
	  expr	{ PUTS("expr"); $$ = op2(NE, $1, valtonode(lookup("$zero&null", symtab, 0), CCON)); }
	| rel_expr		{ PUTS("relexpr"); }
	| lex_expr		{ PUTS("lexexpr"); }
	| compound_conditional	{ PUTS("compcond"); }
	;

else:
	  ELSE optNL	{ PUTS("else"); }
	;

field:
	  FIELD		{ PUTS("field"); $$ = valtonode($1, CFLD); }
	| INDIRECT term { PUTS("ind field"); $$ = op1(INDIRECT, $2); }
	;

if:
	  IF '(' conditional ')' optNL	{ PUTS("if(cond)"); $$ = $3; }
	;

lex_expr:
	  expr MATCHOP regular_expr	{ PUTS("expr~re"); $$ = op2($2, $1, makedfa($3)); }
	| '(' lex_expr ')'	{ PUTS("(lex_expr)"); $$ = $2; }
	;

var:
	  NUMBER	{PUTS("number"); $$ = valtonode($1, CCON); }
	| STRING 	{ PUTS("string"); $$ = valtonode($1, CCON); }
	| VAR		{ PUTS("var"); $$ = valtonode($1, CVAR); }
	| VAR '[' expr ']'	{ PUTS("array[]"); $$ = op2(ARRAY, $1, $3); }
	| field
	;
term:
	  var
	| GETLINE	{ PUTS("getline"); $$ = op1(GETLINE, 0); }
	| FNCN		{ PUTS("func");
			$$ = op2(FNCN, $1, valtonode(lookup("$record", symtab, 0), CFLD));
			}
	| FNCN '(' ')'	{ PUTS("func()"); 
			$$ = op2(FNCN, $1, valtonode(lookup("$record", symtab, 0), CFLD));
			}
	| FNCN '(' expr ')'	{ PUTS("func(expr)"); $$ = op2(FNCN, $1, $3); }
	| SPRINTF print_list	{ PUTS("sprintf"); $$ = op1($1, $2); }
	| SUBSTR '(' expr ',' expr ',' expr ')'
			{ PUTS("substr(e,e,e)"); $$ = op3(SUBSTR, $3, $5, $7); }
	| SUBSTR '(' expr ',' expr ')'
			{ PUTS("substr(e,e,e)"); $$ = op3(SUBSTR, $3, $5, nullstat); }
	| SPLIT '(' expr ',' VAR ',' expr ')'
			{ PUTS("split(e,e,e)"); $$ = op3(SPLIT, $3, $5, $7); }
	| SPLIT '(' expr ',' VAR ')'
			{ PUTS("split(e,e,e)"); $$ = op3(SPLIT, $3, $5, nullstat); }
	| INDEX '(' expr ',' expr ')'
			{ PUTS("index(e,e)"); $$ = op2(INDEX, $3, $5); }
	| '(' expr ')'			{PUTS("(expr)");  $$ = $2; }
	| term '+' term			{ PUTS("t+t"); $$ = op2(ADD, $1, $3); }
	| term '-' term			{ PUTS("t-t"); $$ = op2(MINUS, $1, $3); }
	| term '*' term			{ PUTS("t*t"); $$ = op2(MULT, $1, $3); }
	| term '/' term			{ PUTS("t/t"); $$ = op2(DIVIDE, $1, $3); }
	| term '%' term			{ PUTS("t%t"); $$ = op2(MOD, $1, $3); }
	| '-' term %prec QUEST		{ PUTS("-term"); $$ = op1(UMINUS, $2); }
	| '+' term %prec QUEST		{ PUTS("+term"); $$ = $2; }
	| INCR var	{ PUTS("++var"); $$ = op1(PREINCR, $2); }
	| DECR var	{ PUTS("--var"); $$ = op1(PREDECR, $2); }
	| var INCR	{ PUTS("var++"); $$= op1(POSTINCR, $1); }
	| var DECR	{ PUTS("var--"); $$= op1(POSTDECR, $1); }
	;

expr:
	  term		{ PUTS("term"); }
	| expr term	{ PUTS("expr term"); $$ = op2(CAT, $1, $2); }
	| var ASGNOP expr	{ PUTS("var=expr"); $$ = stat2($2, $1, $3); }
	;

optNL:
	  NL
	|
	;

pa_stat:
	  pattern	{ PUTS("pattern"); $$ = stat2(PASTAT, $1, genprint()); }
	| pattern '{' stat_list '}'	{ PUTS("pattern {...}"); $$ = stat2(PASTAT, $1, $3); }
	| pattern ',' pattern		{ PUTS("srch,srch"); $$ = pa2stat($1, $3, genprint()); }
	| pattern ',' pattern '{' stat_list '}'	
					{ PUTS("srch, srch {...}"); $$ = pa2stat($1, $3, $5); }
	| '{' stat_list '}'	{ PUTS("null pattern {...}"); $$ = stat2(PASTAT, nullstat, $2); }
	;

pa_stats:
	  pa_stats pa_stat st	{ PUTS("pa_stats pa_stat"); $$ = linkum($1, $2); }
	|	{ PUTS("null pa_stat"); $$ = (hack)nullstat; }
	| pa_stats pa_stat	{PUTS("pa_stats pa_stat"); $$ = linkum($1, $2); }
	;

pattern:
	  regular_expr	{ PUTS("regex");
		$$ = op2(MATCH, valtonode(lookup("$record", symtab, 0), CFLD), makedfa($1));
		}
	| rel_expr	{ PUTS("relexpr"); }
	| lex_expr	{ PUTS("lexexpr"); }
	| compound_pattern	{ PUTS("comp pat"); }
	;

print_list:
	  expr	{ PUTS("expr"); }
	| pe_list	{ PUTS("pe_list"); }
	|		{ PUTS("null print_list"); $$ = valtonode(lookup("$record", symtab, 0), CFLD); }
	;

pe_list:
	  expr ',' expr	{$$ = linkum($1, $3); }
	| pe_list ',' expr	{$$ = linkum($1, $3); }
	| '(' pe_list ')'		{$$ = $2; }
	;

redir:
	  RELOP
	| '|'
	;

regular_expr:
	  '/'	{ startreg(); }
	  r '/'
		{ PUTS("/r/"); $$ = $3; }
	;

r:
	  CHAR		{ PUTS("regex CHAR"); $$ = op2(CHAR, (node *) 0, $1); }
	| DOT		{ PUTS("regex DOT"); $$ = op2(DOT, (node *) 0, (node *) 0); }
	| CCL		{ PUTS("regex CCL"); $$ = op2(CCL, (node *) 0, cclenter($1)); }
	| NCCL		{ PUTS("regex NCCL"); $$ = op2(NCCL, (node *) 0, cclenter($1)); }
	| '^'		{ PUTS("regex ^"); $$ = op2(CHAR, (node *) 0, HAT); }
	| '$'		{ PUTS("regex $"); $$ = op2(CHAR, (node *) 0 ,(node *) 0); }
	| r OR r	{ PUTS("regex OR"); $$ = op2(OR, $1, $3); }
	| r r   %prec CAT
			{ PUTS("regex CAT"); $$ = op2(CAT, $1, $2); }
	| r STAR	{ PUTS("regex STAR"); $$ = op2(STAR, $1, (node *) 0); }
	| r PLUS	{ PUTS("regex PLUS"); $$ = op2(PLUS, $1, (node *) 0); }
	| r QUEST	{ PUTS("regex QUEST"); $$ = op2(QUEST, $1, (node *) 0); }
	| '(' r ')'	{ PUTS("(regex)"); $$ = $2; }
	;

rel_expr:
	  expr RELOP expr
		{ PUTS("expr relop expr"); $$ = op2($2, $1, $3); }
	| '(' rel_expr ')'
		{ PUTS("(relexpr)"); $$ = $2; }
	;

st:
	  NL
	| ';'
	;

simple_stat:
	  PRINT print_list redir expr
		{ PUTS("print>stat"); $$ = stat3($1, $2, $3, $4); }
	| PRINT print_list	
		{ PUTS("print list"); $$ = stat3($1, $2, nullstat, nullstat); }
	| PRINTF print_list redir expr
		{ PUTS("printf>stat"); $$ = stat3($1, $2, $3, $4); }
	| PRINTF print_list	
		{ PUTS("printf list"); $$ = stat3($1, $2, nullstat, nullstat); }
	| expr	{ PUTS("expr"); $$ = exptostat($1); }
	|		{ PUTS("null simple statement"); $$ = (hack)nullstat; }
	| error		{ yyclearin; yyerror("illegal statement"); $$ = (hack)nullstat; }
	;

statement:
	  simple_stat st	{ PUTS("simple stat"); }
	| if statement		{ PUTS("if stat"); $$ = stat3(IF, $1, $2, nullstat); }
	| if statement else statement
		{ PUTS("if-else stat"); $$ = stat3(IF, $1, $2, $4); }
	| while statement	{ PUTS("while stat"); $$ = stat2(WHILE, $1, $2); }
	| for			{ PUTS("for stat"); }
	| NEXT st		{ PUTS("next"); $$ = stat1(NEXT, 0); }
	| EXIT st		{ PUTS("exit"); $$ = stat1(EXIT, 0); }
	| EXIT expr st		{ PUTS("exit"); $$ = stat1(EXIT, $2); }
	| BREAK st		{ PUTS("break"); $$ = stat1(BREAK, 0); }
	| CONTINUE st		{ PUTS("continue"); $$ = stat1(CONTINUE, 0); }
	| '{' stat_list '}'	{ PUTS("{statlist}"); $$ = $2; }
	;

stat_list:
	  stat_list statement	{ PUTS("stat_list stat"); $$ = linkum($1, $2); }
	|			{ PUTS("null stat list"); $$ = (hack)nullstat; }
	;

while:
	  WHILE '(' conditional ')' optNL	{ PUTS("while(cond)"); $$ = $3; }
	;

for:
	  FOR '(' simple_stat ';' conditional ';' simple_stat ')' optNL statement
		{ PUTS("for(e;e;e)"); $$ = stat4(FOR, $3, $5, $7, $10); }
	| FOR '(' simple_stat ';'  ';' simple_stat ')' optNL statement
		{ PUTS("for(e;e;e)"); $$ = stat4(FOR, $3, nullstat, $6, $9); }
	| FOR '(' VAR IN VAR ')' optNL statement
		{ PUTS("for(v in v)"); $$ = stat3(IN, $3, $5, $8); }
	;

%%
