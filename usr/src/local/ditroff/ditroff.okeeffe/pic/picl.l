#ifndef lint
static char sccsid[] = "@(#)picl.l	1.1 (CWI) 85/07/19";
#endif lint

%Start A str comment def sc br
%e 1300
%k 100
%a 1400
%o 1200
%p 4000
%n 600

%{
#undef	input
#undef	unput
#include <stdio.h>
#include <ctype.h>
#include "pic.h"
#include "y.tab.h"

extern	float	atof();
extern	struct	symtab	symtab[];
extern	char	*filename;
extern	int	synerr;

#define	CADD	cbuf[clen++]=yytext[0]; if(clen>=CBUFLEN-1) {yyerror("string too long", cbuf); BEGIN A;}
#define	CBUFLEN	150
char	cbuf[CBUFLEN];
int	clen, cflag;
%}

A	[a-zA-Z_]
B	[a-zA-Z0-9_]
D	[0-9]
WS	[ \t]

%%
	switch (yybgin-yysvec-1) {	/* witchcraft */
	case 0:
		BEGIN A;
		break;
	case sc:
		BEGIN A;
		return('}');
	case br:
		BEGIN A;
		return(']');
	}

<A>{WS}		;
<A>"\\"\n	;
<A>\n		{ return(ST); }
<A>";"		{ return(ST); }
<A>"}"		{ BEGIN sc; return(ST); }
<A>"]"		{ BEGIN br; return(ST); }

<A>^".".*	{ if (yytext[1] == 'P' && (yytext[2] == 'E' || yytext[2] == 'F')) {
			yylval.i = yytext[2];
			return(EOF);
		  } else {
			yylval.p = tostring(yytext);
			return(TROFF);
		  }
		}

<A>print	return(yylval.i = PRINT);
<A>box		return(yylval.i = BOX);
<A>circle	return(yylval.i = CIRCLE);
<A>arc		return(yylval.i = ARC);
<A>ellipse	return(yylval.i = ELLIPSE);
<A>arrow	return(yylval.i = ARROW);
<A>spline	return(yylval.i = SPLINE);
<A>line		return(yylval.i = LINE);
<A>move		return(yylval.i = MOVE);
<A>"[]"		return(yylval.i = BLOCK);

<A>same		return(SAME);
<A>between	return(BETWEEN);
<A>and		return(AND);

<A>of		;
<A>the		;
<A>way		;

<A>".e"		{ yylval.i = EAST; return(CORNER); }
<A>".east"	{ yylval.i = EAST; return(CORNER); }
<A>".r"		{ yylval.i = EAST; return(CORNER); }
<A>".right"	{ yylval.i = EAST; return(CORNER); }
<A>".w"		{ yylval.i = WEST; return(CORNER); }
<A>".west"	{ yylval.i = WEST; return(CORNER); }
<A>".l"		{ yylval.i = WEST; return(CORNER); }
<A>".left"	{ yylval.i = WEST; return(CORNER); }
<A>".n"		{ yylval.i = NORTH; return(CORNER); }
<A>".north"	{ yylval.i = NORTH; return(CORNER); }
<A>".t"		{ yylval.i = NORTH; return(CORNER); }
<A>".top"	{ yylval.i = NORTH; return(CORNER); }
<A>".s"		{ yylval.i = SOUTH; return(CORNER); }
<A>".south"	{ yylval.i = SOUTH; return(CORNER); }
<A>".b"		{ yylval.i = SOUTH; return(CORNER); }
<A>".bot"	{ yylval.i = SOUTH; return(CORNER); }
<A>".bottom"	{ yylval.i = SOUTH; return(CORNER); }
<A>".c"		{ yylval.i = CENTER; return(CORNER); }
<A>".center"	{ yylval.i = CENTER; return(CORNER); }
<A>".start"	{ yylval.i = START; return(CORNER); }
<A>".end"	{ yylval.i = END; return(CORNER); }
<A>".ne"		{ yylval.i = NE; return(CORNER); }
<A>"."upper" "*right	{ yylval.i = NE; return(CORNER); }
<A>".se"		{ yylval.i = SE; return(CORNER); }
<A>"."lower" "*right	{ yylval.i = SE; return(CORNER); }
<A>".nw"		{ yylval.i = NW; return(CORNER); }
<A>"."upper" "*left	{ yylval.i = NW; return(CORNER); }
<A>".sw"		{ yylval.i = SW; return(CORNER); }
<A>"."lower" "*left	{ yylval.i = SW; return(CORNER); }

<A>top" "+of		{ yylval.i = NORTH; return(CORNER); }
<A>north" "+of		{ yylval.i = NORTH; return(CORNER); }
<A>bottom" "+of		{ yylval.i = SOUTH; return(CORNER); }
<A>south" "+of		{ yylval.i = SOUTH; return(CORNER); }
<A>left" "+of		{ yylval.i = WEST; return(CORNER); }
<A>west" "+of		{ yylval.i = WEST; return(CORNER); }
<A>right" "+of		{ yylval.i = EAST; return(CORNER); }
<A>east" "+of		{ yylval.i = EAST; return(CORNER); }
<A>center" "+of		{ yylval.i = CENTER; return(CORNER); }
<A>start" "+of		{ yylval.i = START; return(CORNER); }
<A>end" "+of		{ yylval.i = END; return(CORNER); }
<A>upper" "+right" "+of	{ yylval.i = NE; return(CORNER); }
<A>upper" "+left" "+of	{ yylval.i = NW; return(CORNER); }
<A>lower" "+right" "+of	{ yylval.i = SE; return(CORNER); }
<A>lower" "+left" "+of	{ yylval.i = SW; return(CORNER); }

<A>height	{ yylval.i = HEIGHT; return(ATTR); }
<A>ht		{ yylval.i = HEIGHT; return(ATTR); }
<A>wid		{ yylval.i = WIDTH; return(ATTR); }
<A>width	{ yylval.i = WIDTH; return(ATTR); }
<A>rad		{ yylval.i = RADIUS; return(ATTR); }
<A>radius	{ yylval.i = RADIUS; return(ATTR); }
<A>diam		{ yylval.i = DIAMETER; return(ATTR); }
<A>diameter	{ yylval.i = DIAMETER; return(ATTR); }
<A>size		{ yylval.i = SIZE; return(ATTR); }
<A>left		{ yylval.i = LEFT; return(DIR); }
<A>right	{ yylval.i = RIGHT; return(DIR); }
<A>up		{ yylval.i = UP; return(DIR); }
<A>down		{ yylval.i = DOWN; return(DIR); }
<A>cw		{ yylval.i = CW; return(ATTR); }
<A>clockwise	{ yylval.i = CW; return(ATTR); }
<A>ccw		{ yylval.i = CCW; return(ATTR); }
<A>then		{ yylval.i = THEN; return(ATTR); }
<A>invis	{ yylval.i = INVIS; return(ATTR); }
<A>invisible	{ yylval.i = INVIS; return(ATTR); }
<A>dot		return(yylval.i = DOT);
<A>dotted	return(yylval.i = DOT);
<A>dash		return(yylval.i = DASH);
<A>dashed	return(yylval.i = DASH);
<A>chop		return(yylval.i = CHOP);

<A>spread	return(yylval.i = SPREAD);
<A>fill		return(yylval.i = FILL);
<A>ljust	return(yylval.i = LJUST);
<A>rjust	return(yylval.i = RJUST);
<A>above	return(yylval.i = ABOVE);
<A>below	return(yylval.i = BELOW);

<A>"<-"		{ yylval.i = HEAD1; return(HEAD); }
<A>"->"		{ yylval.i = HEAD2; return(HEAD); }
<A>"<->"	{ yylval.i = HEAD12; return(HEAD); }

<A>".x"		return(yylval.i = DOTX);
<A>".y"		return(yylval.i = DOTY);
<A>".ht"	return(yylval.i = DOTHT);
<A>".height"	return(yylval.i = DOTHT);
<A>".wid"	return(yylval.i = DOTWID);
<A>".width"	return(yylval.i = DOTWID);
<A>".rad"	return(yylval.i = DOTRAD);
<A>".radius"	return(yylval.i = DOTRAD);

<A>from		return(yylval.i = FROM);
<A>to		return(yylval.i = TO);
<A>at		return(yylval.i = AT);
<A>by		return(yylval.i = BY);
<A>with		return(yylval.i = WITH);
<A>last		return(yylval.i = LAST);

<A>Here		return(yylval.i = HERE);
<A>define{WS}+	{ BEGIN def; }
<def>{A}{B}*	{ definition(yytext); BEGIN A; }

<A>first		{ yylval.i = 1; return(NTH); }
<A>{D}+(th|nd|rd|st)	{ yylval.i = atoi(yytext); return(NTH); }
<A>({D}+("."?){D}*|"."{D}+)i?	{ yylval.f = atof(yytext); return(NUMBER); }

<A>{A}{B}* {
		int c;
		struct symtab *p;
		p = lookup(yytext);
		if (p != NULL && p->s_type == DEFNAME) {
			c = input();
			unput(c);
			if (c == '(')	/* it's name(...) */
				dodef(p);
			else {	/* no argument list */
				pbstr(p->s_val);
				dprintf("pushing back `%s'\n", p->s_val);
			}
		} else if (islower(yytext[0])) {
			yylval.p = tostring(yytext);
			return(VARNAME);
		} else {
			yylval.p = tostring(yytext);
			return(PLACENAME);
		}
	}

<A>\"		{ BEGIN str; clen=0; }

<A>#		{ BEGIN comment; }
<comment>\n	{ BEGIN A; return(ST); }
<comment>.	;

<A>.		{ yylval.i = yytext[0]; return(yytext[0]); }

<str>\"		{ BEGIN A; cbuf[clen]=0; yylval.p = tostring(cbuf); return(TEXT); }
<str>\n		{ yyerror("newline in string"); BEGIN A; return(ST); }
<str>"\\\""	{ cbuf[clen++]='"'; }
<str>"\\"t	{ cbuf[clen++]='\t'; }
<str>"\\\\"	{ cbuf[clen++]='\\'; }
<str>.		{ CADD; }

%%
