/*	awk.lx.l	4.4	90/06/25	*/

%X str chc reg comment

%{
#include	<string.h>
#include	"awk.h"
#include	"awk.def"
extern int	yylval;
extern int	mustfld;
extern int	ldbg;
extern char	*lexprog;

#undef YY_INPUT
#define YY_INPUT(buf,result,max_size)				\
{								\
	if ( lexprog )						\
		{						\
		result = strlen( lexprog );			\
		if ( result > max_size )			\
			{					\
			result = max_size;			\
			strncpy( buf, lexprog, result );	\
			}					\
		else						\
			strcpy( buf, lexprog );			\
		lexprog += result;				\
		}						\
	else							\
		result = read( fileno(yyin), buf, max_size );	\
}

int	lineno	= 1;
#define	RETURN(x)	{if (ldbg) ptoken(x); return(x); }
#define	CADD	cbuf[clen++]=yytext[0]; if(clen>=CBUFLEN-1) {yyerror("string too long", cbuf); BEGIN INITIAL;}
#define	CBUFLEN	150
char	cbuf[CBUFLEN];
int	clen, cflag;
%}

A	[a-zA-Z_]
B	[a-zA-Z0-9_]
D	[0-9]
WS	[ \t]

%%
	static int sc_flag = 0;

	if ( sc_flag ) {
		BEGIN INITIAL;
		sc_flag = 0;
		RETURN('}');
	}

^\n		lineno++;
^{WS}*#.*\n	lineno++;	/* strip comment lines */
{WS}		;
<INITIAL,reg>"\\"\n	lineno++;
"||"		RETURN(BOR);
BEGIN	RETURN(XBEGIN);
END		RETURN(XEND);
PROGEND	RETURN(EOF);
"&&"		RETURN(AND);
"!"		RETURN(NOT);
"!="		{ yylval = NE; RETURN(RELOP); }
"~"		{ yylval = MATCH; RETURN(MATCHOP); }
"!~"		{ yylval = NOTMATCH; RETURN(MATCHOP); }
"<"		{ yylval = LT; RETURN(RELOP); }
"<="		{ yylval = LE; RETURN(RELOP); }
"=="		{ yylval = EQ; RETURN(RELOP); }
">="		{ yylval = GE; RETURN(RELOP); }
">"		{ yylval = GT; RETURN(RELOP); }
">>"		{ yylval = APPEND; RETURN(RELOP); }
"++"		{ yylval = INCR; RETURN(INCR); }
"--"		{ yylval = DECR; RETURN(DECR); }
"+="		{ yylval = ADDEQ; RETURN(ASGNOP); }
"-="		{ yylval = SUBEQ; RETURN(ASGNOP); }
"*="		{ yylval = MULTEQ; RETURN(ASGNOP); }
"/="		{ yylval = DIVEQ; RETURN(ASGNOP); }
"%="		{ yylval = MODEQ; RETURN(ASGNOP); }
"="		{ yylval = ASSIGN; RETURN(ASGNOP); }

"$"{D}+	{	if (atoi(yytext+1)==0) {
				yylval = (hack)lookup("$record", symtab, 0);
				RETURN(STRING);
			} else {
				yylval = fieldadr(atoi(yytext+1));
				RETURN(FIELD);
			}
		}
"$"{WS}*	{ RETURN(INDIRECT); }
NF		{ mustfld=1; yylval = (hack)setsymtab(yytext, EMPTY, 0.0, NUM, symtab); RETURN(VAR); }
({D}+("."?){D}*|"."{D}+)((e|E)("+"|-)?{D}+)?	{
		yylval = (hack)setsymtab(yytext, EMPTY, atof(yytext), CON|NUM, symtab); RETURN(NUMBER); }
"}"{WS}*\n	{ sc_flag = 1; lineno++; RETURN(';'); }
"}"		{ sc_flag = 1; RETURN(';'); }
;\n		{ lineno++; RETURN(';'); }
\n		{ lineno++; RETURN(NL); }
while	RETURN(WHILE);
for		RETURN(FOR);
if		RETURN(IF);
else		RETURN(ELSE);
next		RETURN(NEXT);
exit		RETURN(EXIT);
break	RETURN(BREAK);
continue	RETURN(CONTINUE);
print	{ yylval = PRINT; RETURN(PRINT); }
printf	{ yylval = PRINTF; RETURN(PRINTF); }
sprintf	{ yylval = SPRINTF; RETURN(SPRINTF); }
split	{ yylval = SPLIT; RETURN(SPLIT); }
substr	RETURN(SUBSTR);
index	RETURN(INDEX);
in		RETURN(IN);
getline	RETURN(GETLINE);
length	{ yylval = FLENGTH; RETURN(FNCN); }
log		{ yylval = FLOG; RETURN(FNCN); }
int		{ yylval = FINT; RETURN(FNCN); }
exp		{ yylval = FEXP; RETURN(FNCN); }
sqrt		{ yylval = FSQRT; RETURN(FNCN); }
{A}{B}*	{ yylval = (hack)setsymtab(yytext, tostring(""), 0.0, STR|NUM, symtab); RETURN(VAR); }
\"		{ BEGIN str; clen=0; }

#		{ BEGIN comment; }
<comment>\n	{ BEGIN INITIAL; lineno++; RETURN(NL); }
<comment>.	;

.		{ yylval = yytext[0]; RETURN(yytext[0]); }

<reg>"["	{ BEGIN chc; clen=0; cflag=0; }
<reg>"[^"	{ BEGIN chc; clen=0; cflag=1; }

<reg>"?"	RETURN(QUEST);
<reg>"+"	RETURN(PLUS);
<reg>"*"	RETURN(STAR);
<reg>"|"	RETURN(OR);
<reg>"."	RETURN(DOT);
<reg>"("	RETURN('(');
<reg>")"	RETURN(')');
<reg>"^"	RETURN('^');
<reg>"$"	RETURN('$');
<reg>\\{D}{D}{D}	{ sscanf(yytext+1, "%o", &yylval); RETURN(CHAR); }
<reg>\\.	{	if (yytext[1]=='n') yylval = '\n';
			else if (yytext[1] == 't') yylval = '\t';
			else yylval = yytext[1];
			RETURN(CHAR);
		}
<reg>"/"	{ BEGIN INITIAL; unput('/'); }
<reg>\n		{ yyerror("newline in regular expression"); lineno++; BEGIN INITIAL; }
<reg>.		{ yylval = yytext[0]; RETURN(CHAR); }

<str>\"		{ char *s; BEGIN INITIAL; cbuf[clen]=0; s = tostring(cbuf);
		cbuf[clen] = ' '; cbuf[++clen] = 0;
		yylval = (hack)setsymtab(cbuf, s, 0.0, CON|STR, symtab); RETURN(STRING); }
<str>\n		{ yyerror("newline in string"); lineno++; BEGIN INITIAL; }
<str>"\\\""	{ cbuf[clen++]='"'; }
<str,chc>"\\"n	{ cbuf[clen++]='\n'; }
<str,chc>"\\"t	{ cbuf[clen++]='\t'; }
<str,chc>"\\\\"	{ cbuf[clen++]='\\'; }
<str>.		{ CADD; }

<chc>"\\""]"	{ cbuf[clen++]=']'; }
<chc>"]"	{ BEGIN reg; cbuf[clen]=0; yylval = (hack)tostring(cbuf);
		if (cflag==0) { RETURN(CCL); }
		else { RETURN(NCCL); } }
<chc>\n		{ yyerror("newline in character class"); lineno++; BEGIN INITIAL; }
<chc>.		{ CADD; }

%%

startreg()
{
	BEGIN reg;
}

ptoken(n)
{
	extern struct tok {
		char *tnm;
		int yval;
	} tok[];
	extern int yylval;

	printf("lex:");
	if (n < 128) {
		printf(" %c\n",n);
		return;
	}
	if (n <= 256 || n >= LASTTOKEN) {
		printf("? %o\n",n);
		return;
	}
	printf(" %s",tok[n-257].tnm);
	switch (n) {

	case RELOP:
	case MATCHOP:
	case ASGNOP:
	case STRING:
	case FIELD:
	case VAR:
	case NUMBER:
	case FNCN:
		printf(" (%s)", yytext);
		break;

	case CHAR:
		printf(" (%o)", yylval);
		break;
	}
	putchar('\n');
}
