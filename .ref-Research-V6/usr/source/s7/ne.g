\{#
#
int	fromflg	0;
\}
%term	CONTIG QTEXT  SPACE THIN TAB
%term	UP DOWN BACK FWD
%term	MATRIX LCOL CCOL RCOL COL
%term	MARK LINEUP
%term	SUM INT PROD UNION INTER
%term	LPILE PILE CPILE RPILE ABOVE
%term	DEFINE DELIM  GSIZE GFONT
%right	FROM TO
%left	OVER SQRT
%right	SUPER SUB
%right	SIZE FONT ROMAN ITALIC BOLD
%left	MQ MQ1
%left	LEFT RIGHT
%right	DOT DOTDOT HAT TILDE BAR UNDER

%%

stuff	: eqn 	={ putout($1); }
	| error	={ error(!FATAL, "syntax error in equation %d", first);  }
	|	={ eqnreg = 0; }
	;

eqn	: box	={ if(dbg)printf(".\teqn: S%d\n",$1); }
	| eqn box	={ eqnbox($1,$2); }
	| eqn MARK	={ mark($1); }
	| MARK	={ mark(0); }
	| eqn LINEUP	={ lineup($1); }
	| LINEUP	={ lineup(0); }
	;

matrix	: MATRIX	={ $$ = ct; } ;

collist	: column
	| collist column
	;

column	: lcol MQ list MQ1	={ column('L',$1,$3); }
	| ccol MQ list MQ1	={ column('C',$1,$3); }
	| rcol MQ list MQ1	={ column('R',$1,$3); }
	| col MQ list MQ1	={ column('-',$1,$3); }
	;

lcol	: LCOL	={ $$ = ct++; } ;
ccol	: CCOL	={ $$ = ct++; } ;
rcol	: RCOL	={ $$ = ct++; } ;
col	: COL	={ $$ = ct++; } ;

sbox	: super box	%prec SUPER
		={ $$ = $2; }
	|	%prec SUB
		={ $$ = 0; }
	;

box	: box OVER box	={ boverb($1,$3); }
	| size box	%prec SIZE
		={ size($1,$2); }
	| font box	%prec FONT
		={ font($1,$2); }
	| SQRT box	={ sqrt($2); }
	| lpile MQ list MQ1	%prec LPILE
		={ lpile('L', $1, ct); ct=$1; }
	| cpile MQ list MQ1	%prec CPILE
		={ lpile('C', $1, ct); ct=$1; }
	| rpile MQ list MQ1	%prec RPILE
		={ lpile('R', $1, ct); ct=$1; }
	| pile MQ list MQ1	%prec PILE
		={ lpile('-', $1, ct); ct=$1; }
	| box sub box sbox	%prec SUB
		={ if(dbg)printf(".\t sub box %d %d %d\n",$1,$3,$4);
			if($4 == 0)bshiftb($1,$2,$3);
			else shift2($1,$3,$4);
		}
	| box super box		%prec SUPER
		={ bshiftb($1,$2,$3); }
	| left eqn right	%prec LEFT
		={ paren($1,$2,$3);	}
	| pbox
	| box from box to  box	%prec TO
		={ fromto($1,$3,$5); fromflg=0; }
	| box from box	%prec FROM
		={ fromto($1,$3,0); fromflg=0; }
	| box to box	%prec TO
		={ fromto($1,0,$3); }
	| box diacrit	={ diacrit($1,$2); }
	| fwd box	={ move(0,$1,$2); }
	| up box	={ move(1,$1,$2); }
	| back box	={ move(2,$1,$2); }
	| down box	={ move(3,$1,$2); }
	| matrix MQ collist MQ1	={ matrix($1,$3); }
	;

fwd	: FWD text	={ $$ = numb($1); } ;
up	: UP text	={ $$ = numb($1); } ;
back	: BACK text	={ $$ = numb($1); } ;
down	: DOWN text	={ $$ = numb($1); } ;

diacrit	: HAT	={ $$ = 'H'; }
	| BAR	={ $$ = 'B'; }
	| UNDER	={ $$ = 'N'; }	/* under bar */
	| DOT	={ $$ = 'D'; }
	| TILDE	={ $$ = 'T'; }
	| DOTDOT	={ $$ = 'U'; } /* umlaut = double dot */
	;

from	: FROM
	;
to	: TO
	;

left	: LEFT text	={ $$ = $2->c1; }
	| LEFT MQ	={ $$ = '{'; }
	;

right	: RIGHT text	={ $$ = $2->c1; }
	| RIGHT MQ1	={ $$ = '}'; }
	|		={ $$ = 0; }
	;

list	: eqn	={ lp[ct++] = $1; }
	| list ABOVE eqn	={ lp[ct++] = $3; }
	;

lpile	: LPILE	={ $$=ct; } ;
cpile	: CPILE	={ $$=ct; } ;
pile	: PILE	={ $$=ct; } ;
rpile	: RPILE	={ $$=ct; } ;

size	: SIZE text
	;

font	: ROMAN
	| ITALIC
	| BOLD
	| FONT text
	;

sub	: SUB	={ shift(1); } ;

super	: SUPER	={ shift(-1); } ;

pbox	: MQ eqn MQ1	={ $$ = $2; }
	| QTEXT	={ text('q',$1); }
	| CONTIG	={ text('c',$1); }
	| SPACE	={ text('~', $1); }
	| THIN	={ text('^', $1); }
	| TAB	={ text('\t', $1); }
	| SUM	={	text('c', "SIGMA"); }
	| PROD	={	text('c', "PI"); }
	| INT	={	text('c', "^"); }
	| UNION	={	text('c' ,"U"); }
	| INTER	={	text('c', "A"); }
	;

text	: CONTIG
	| QTEXT
	| SPACE	={ $$ = & "\\|\\|"; }
	| THIN	={ $$ = & "\\|"; }
	;

%%
