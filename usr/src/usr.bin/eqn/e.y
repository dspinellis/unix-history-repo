%{#
/*	e.y	4.1	83/02/11	*/
#include "e.h"
#
int	fromflg;
%}
%term	CONTIG QTEXT SPACE THIN TAB
%term	MATRIX LCOL CCOL RCOL COL
%term	MARK LINEUP
%term	SUM INT PROD UNION INTER
%term	LPILE PILE CPILE RPILE ABOVE
%term	DEFINE TDEFINE NDEFINE DELIM GSIZE GFONT INCLUDE
%right	FROM TO
%left	OVER SQRT
%right	SUP SUB
%right	SIZE FONT ROMAN ITALIC BOLD FAT
%right	UP DOWN BACK FWD
%left	LEFT RIGHT
%right	DOT DOTDOT HAT TILDE BAR UNDER VEC DYAD

%%

stuff	: eqn 	{ putout($1); }
	| error	{ error(!FATAL, "syntax error"); }
	|	{ eqnreg = 0; }
	;

eqn	: box
	| eqn box	{ eqnbox($1, $2, 0); }
	| eqn lineupbox	{ eqnbox($1, $2, 1); }
	| LINEUP	{ lineup(0); }
	;

lineupbox: LINEUP box	{ $$ = $2; lineup(1); }
	;

matrix	: MATRIX	{ $$ = ct; } ;

collist	: column
	| collist column
	;

column	: lcol '{' list '}'	{ column('L', $1); }
	| ccol '{' list '}'	{ column('C', $1); }
	| rcol '{' list '}'	{ column('R', $1); }
	| col '{' list '}'	{ column('-', $1); }
	;

lcol	: LCOL		{ $$ = ct++; } ;
ccol	: CCOL		{ $$ = ct++; } ;
rcol	: RCOL		{ $$ = ct++; } ;
col	: COL		{ $$ = ct++; } ;

sbox	: sup box	%prec SUP	{ $$ = $2; }
	;

tbox	: to box	%prec TO	{ $$ = $2; }
	|		%prec FROM	{ $$ = 0; }
	;

box	: box OVER box	{ boverb($1, $3); }
	| MARK box	{ mark($2); }
	| size box	%prec SIZE	{ size($1, $2); }
	| font box	%prec FONT	{ font($1, $2); }
	| FAT box	{ fatbox($2); }
	| SQRT box	{ sqrt($2); }
	| lpile '{' list '}'	{ lpile('L', $1, ct); ct = $1; }
	| cpile '{' list '}'	{ lpile('C', $1, ct); ct = $1; }
	| rpile '{' list '}'	{ lpile('R', $1, ct); ct = $1; }
	| pile '{' list '}'	{ lpile('-', $1, ct); ct = $1; }
	| box sub box sbox	%prec SUB	{ shift2($1, $3, $4); }
	| box sub box		%prec SUB	{ bshiftb($1, $2, $3); }
	| box sup box		%prec SUP	{ bshiftb($1, $2, $3); }
	| int sub box sbox	%prec SUB	{ integral($1, $3, $4); }
	| int sub box		%prec SUB	{ integral($1, $3, 0); }
	| int sup box		%prec SUP	{ integral($1, 0, $3); }
	| int					{ integral($1, 0, 0); }
	| left eqn right	{ paren($1, $2, $3); }
	| pbox
	| box from box tbox	%prec FROM	{ fromto($1, $3, $4); fromflg=0; }
	| box to box	%prec TO	{ fromto($1, 0, $3); }
	| box diacrit	{ diacrit($1, $2); }
	| fwd box	%prec UP	{ move(FWD, $1, $2); }
	| up box	%prec UP	{ move(UP, $1, $2); }
	| back box	%prec UP	{ move(BACK, $1, $2); }
	| down box	%prec UP	{ move(DOWN, $1, $2); }
	| matrix '{' collist '}'	{ matrix($1); }
	;

int	: INT	{ setintegral(); }
	;

fwd	: FWD text	{ $$ = atoi((char *) $1); } ;
up	: UP text	{ $$ = atoi((char *) $1); } ;
back	: BACK text	{ $$ = atoi((char *) $1); } ;
down	: DOWN text	{ $$ = atoi((char *) $1); } ;

diacrit	: HAT	{ $$ = HAT; }
	| VEC	{ $$ = VEC; }
	| DYAD	{ $$ = DYAD; }
	| BAR	{ $$ = BAR; }
	| UNDER	{ $$ = UNDER; }	/* under bar */
	| DOT	{ $$ = DOT; }
	| TILDE	{ $$ = TILDE; }
	| DOTDOT	{ $$ = DOTDOT; } /* umlaut = double dot */
	;

from	: FROM	{ $$=ps; ps -= 3; fromflg = 1;
		if(dbg)printf(".\tfrom: old ps %d, new ps %d, fflg %d\n", $$, ps, fromflg);
		}
	;

to	: TO	{ $$=ps; if(fromflg==0)ps -= 3; 
			if(dbg)printf(".\tto: old ps %d, new ps %d\n", $$, ps);
		}
	;

left	: LEFT text	{ $$ = ((char *)$2)[0]; }
	| LEFT '{'	{ $$ = '{'; }
	;

right	: RIGHT text	{ $$ = ((char *)$2)[0]; }
	| RIGHT '}'	{ $$ = '}'; }
	|		{ $$ = 0; }
	;

list	: eqn	{ lp[ct++] = $1; }
	| list ABOVE eqn	{ lp[ct++] = $3; }
	;

lpile	: LPILE	{ $$ = ct; } ;
cpile	: CPILE	{ $$ = ct; } ;
pile	: PILE	{ $$ = ct; } ;
rpile	: RPILE	{ $$ = ct; } ;

size	: SIZE text	{ $$ = ps; setsize((char *) $2); }
	;

font	: ROMAN		{ setfont(ROM); }
	| ITALIC	{ setfont(ITAL); }
	| BOLD		{ setfont(BLD); }
	| FONT text	{ setfont(((char *)$2)[0]); }
	;

sub	: SUB	{ shift(SUB); }
	;

sup	: SUP	{ shift(SUP); }
	;

pbox	: '{' eqn '}'	{ $$ = $2; }
	| QTEXT		{ text(QTEXT, (char *) $1); }
	| CONTIG	{ text(CONTIG, (char *) $1); }
	| SPACE		{ text(SPACE, 0); }
	| THIN		{ text(THIN, 0); }
	| TAB		{ text(TAB, 0); }
	| SUM		{ funny(SUM); }
	| PROD		{ funny(PROD); }
	| UNION		{ funny(UNION); }
	| INTER		{ funny(INTER); }	/* intersection */
	;

text	: CONTIG
	| QTEXT
	;

%%
