%Start A str def thru sh

%{
#undef	input
#undef	unput
#include <stdio.h>
#include <ctype.h>
#include "grap.h"
#include "y.tab.h"

extern	struct	symtab	symtab[];

#define	CADD	cbuf[clen++]=yytext[0]; if(clen>=CBUFLEN-1) {yyerror("string too long", cbuf); BEGIN A;}
#define	CBUFLEN	1500
char	cbuf[CBUFLEN];
int	clen, cflag;
int	c, delim;
%}

A	[a-zA-Z_]
B	[a-zA-Z0-9_]
D	[0-9]
WS	[ \t]

%%
	if (yybgin-yysvec-1 == 0) {	/* witchcraft */
		BEGIN A;
	}

<A>{WS}		;
<A>"\\"\n	;
<A>\n		return(ST);
<A>";"		return(ST);

<A>line		return(yylval.i = LINE);
<A>arrow	{ yylval.i = ARROW; return(LINE); }
<A>circle	return(yylval.i = CIRCLE);
<A>frame	return(FRAME);
<A>tick(s)?	return(TICKS);
<A>grid(line)?(s)?	return(GRID);
<A>coord(s)?	return(COORD);
<A>log		return(LOG);
<A>exp		return(EXP);
<A>sin		return(SIN);
<A>cos		return(COS);
<A>atan2	return(ATAN2);
<A>sqrt		return(SQRT);
<A>rand		return(RAND);
<A>max		return(MAX);
<A>min		return(MIN);
<A>int		return(INT);
<A>print	return(PRINT);
<A>pic{WS}.*	{ yylval.p = tostring(yytext+3); return(PIC); }
<A>graph{WS}.*	{ yylval.p = tostring(yytext+5); return(GRAPH); }

<A>for		return(FOR);
<A>^Endfor\n	{ endfor(); }
<A>do		{ yylval.p = delimstr("loop body"); BEGIN A; return(DOSTR); }

<A>copy|include	{ return(COPY); }
<A>thru|through	{ BEGIN thru; return(THRU); }
<thru>{WS}+	;
<thru>{A}{B}*|.	{ yylval.op = copythru(yytext); BEGIN A; return(DEFNAME); }
<A>until	return(UNTIL);

<A>if		return(IF);
<A>then		{ yylval.p = delimstr("then part"); BEGIN A; return(THEN); }
<A>else		{ yylval.p = delimstr("else part"); BEGIN A; return(ELSE); }

<A>next		return(NEXT);
<A>draw		return(yylval.i = DRAW);
<A>new		return(yylval.i = NEW);
<A>plot		return(yylval.i = PLOT);
<A>label(s)?	return(LABEL);
<A>x		return(X);
<A>y		return(Y);

<A>top		{ yylval.i = TOP; return SIDE; }
<A>bot(tom)?	{ yylval.i = BOT; return SIDE; }
<A>left		{ yylval.i = LEFT; return SIDE; }
<A>right	{ yylval.i = RIGHT; return SIDE; }
<A>up		return(yylval.i = UP);
<A>down		return(yylval.i = DOWN);
<A>across	return(yylval.i = ACROSS);
<A>height|ht	return(yylval.i = HEIGHT);
<A>wid(th)?	return(yylval.i = WIDTH);
<A>rad(ius)?	return(yylval.i = RADIUS);
<A>invis	return(yylval.i = INVIS);
<A>dot(ted)	return(yylval.i = DOT);
<A>dash(ed)	return(yylval.i = DASH);
<A>solid	return(yylval.i = SOLID);

<A>ljust	{ yylval.i = LJUST; return JUST; }
<A>rjust	{ yylval.i = RJUST; return JUST; }
<A>above	{ yylval.i = ABOVE; return JUST; }
<A>below	{ yylval.i = BELOW; return JUST; }
<A>size		return(yylval.i = SIZE);

<A>from		return(yylval.i = FROM);
<A>to		return(yylval.i = TO);
<A>by|step	return(yylval.i = BY);
<A>at		return(yylval.i = AT);
<A>with		return(yylval.i = WITH);
<A>in		return(yylval.i = IN);
<A>out		return(yylval.i = OUT);
<A>off		return(yylval.i = OFF);

<A>sh{WS}+ {	BEGIN sh;
		if ((delim = input()) == '{') delim = '}';	/* no nested {} */
		shell_init(); }
<sh>{A}{B}* {
		int c;
		Obj *p;
		if (yytext[0] == delim) {
			shell_exec();
			BEGIN A;
		} else {
			p = lookup(yytext, 0);
			if (p != NULL && p->type == DEFNAME) {
				c = input();
				unput(c);
				if (c == '(')
					dodef(p);
				else
					pbstr(p->val);
			} else
				shell_text(yytext);
		}
	}
<sh>.|\n	{ if (yytext[0] == delim) {
			shell_exec();
			BEGIN A;
		  } else
			shell_text(yytext);
		}

<A>define{WS}+	{ BEGIN def; }
<def>{A}{B}*	{ definition(yytext); BEGIN A; }

<A>({D}+("."?){D}*|"."{D}+)((e|E)("+"|-)?{D}+)?i? {
		  yylval.f = atof(yytext); return(NUMBER); }

<A>^"."[^{D}].*	{ if (yytext[1] == 'G' && yytext[2] == '2') {
			yylval.i = yytext[2];
			return(EOF);
		  } else {
			yylval.p = tostring(yytext);
			return(PIC);
		  }
		}

<A>{A}{B}* {
		int c;
		Obj *p;
		p = lookup(yytext, 1);
		if (p->type == DEFNAME) {
			c = input();
			unput(c);
			if (c == '(')	/* it's name(...) */
				dodef(p);
			else	/* no argument list */
				pbstr(p->val);
		} else {
			yylval.op = p;
			return p->type;	/* NAME or VARNAME */
		}
	}

<A>"=="		return(EQ);
<A>">="		return(GE);
<A>"<="		return(LE);
<A>"!="		return(NE);
<A>">"		return(GT);
<A>"<"		return(LT);
<A>"&&"		return(AND);
<A>"||"		return(OR);
<A>"!"		return(NOT);	

<A>\"		{ BEGIN str; clen = 0; }

<A>#.*		;

<A>.		{ yylval.i = yytext[0]; return(yytext[0]); }

<str>\"		{ BEGIN A; cbuf[clen] = 0;
		  yylval.p = tostring(cbuf); return(STRING); }
<str>\n		{ yyerror("newline in string"); BEGIN A; return(ST); }
<str>"\\\""	{ cbuf[clen++] = '\\'; cbuf[clen++] = '"'; }
<str>"\\\\"	{ cbuf[clen++] = '\\'; cbuf[clen++] = '\\'; }
<str>.		{ CADD; }

%%
