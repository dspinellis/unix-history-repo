%term	lex0, lex1, lex2, lex3, lex4, lex5, lex6
%term	lpar, rpar, lbkt, rbkt, eol, unk
%term	com, com0, strng, null, dot, cln
%term	quad, semi, comnt, tran, asg
%term	nam, numb, nfun, mfun, dfun
%term	comexpr, comnam, comnull

%term		dscal,	mdscal
%term	m,	d,	md
%term	msub,	mdsub,

%{
#include "apl.h"
	int	vcount;
	int	scount;
	int	litflag;
	int	nlexsym;
	int	context;
	unsigned	char	*iline;
	char	*ccharp;
%}

%%

/*
 * line-at-a-time APL compiler.
 * first lexical character gives context.
 */
line:

/*
 * immediate.
 */
    lex0 stat =
	{
		integ = ccharp[-1];
		if(integ != ASGN && integ != PRINT)
			*ccharp++ = PRINT;
		*ccharp++ = EOL;
	} |
    lex0 bcomand comand eol =
	{
		*ccharp++ = IMMED;
		*ccharp++ = $3;
	} |
/*
 * quad
 */
    lex1 stat |
/*
 * function definition
 */
    lex2 func |
/*
 * function prolog
 */
    lex3 func |
/*
 * function epilog
 */
    lex4 func |
/*
 * function body
 */
    lex5 fstat ;









/*
 * function header
 */
func:
    anyname asg header =
	{
		switch(context) {

		case lex3:
			name($$, AUTO);
			*ccharp++ = ELID;
			break;

		case lex4:
			integ = ccharp;
			*ccharp++ = EOL;
			name($$, NAME);
			name($$, REST);
			invert($3, integ);
		}
	} |
    header =
	{
		if(context == lex3)
			*ccharp++ = ELID;
	} ;
header:
    args autos =
	{
		if(context == lex4)
			invert($$, $2);
	} ;

args:
    anyname anyname anyname =
	{
		$$ = ccharp;
		switch(context) {

		case lex2:
			name($2, DF);
			break;

		case lex3:
			name($1, ARG1);
			name($3, ARG2);
			break;

		case lex4:
			name($1, REST);
			name($3, REST);
		}
	} |
    anyname anyname =
	{
		$$ = ccharp;
		switch(context) {

		case lex2:
			name($1, MF);
			break;

		case lex3:
			name($2, ARG1);
			break;

		case lex4:
			name($2, REST);
		}
	} |
    anyname =
	{
		if(context == lex2)
			name($$, NF);
		$$ = ccharp;
	} ;
autos:
    semi nam autos =
	{
		$$ = $3;
		switch(context) {

		case lex3:
			name($2, AUTO);
			break;

		case lex4:
			integ = name($2, REST);
			invert($$, integ);
		}
	} |
    eol =
	{
		$$ = ccharp;
	} ;

/*
 * system commands
 */
bcomand:
    rpar =
	{
		litflag = -1;
	} ;
comand:
    comexpr expr |
    comnam anyname =
	{
		name($2, NAME);
	} |
    comnull ;

/*
 * statement:
 *	comments
 *	expressions
 *	heterogeneous output
 *	transfers (in functions)
 */
fstat:
    numb cln realfstat = {
	$$ = $3;
    } |
    realfstat = $$ = $1;

realfstat:
    stat |
    tran eol =
	{
		$$ = ccharp;
		*ccharp++ = BRAN0;
	} |
    tran expr eol =
	{
		$$ = $2;
		*ccharp++ = BRAN;
	} ;
stat:
    statement eol ;
statement:
    comnt =
	{
		litflag = 1;
		$$ = ccharp;
		*ccharp++ = COMNT;
	} |
    expr |
    hprint ;
hprint:
    expr hsemi output ;
output:
    expr =
	{
		*ccharp++ = PRINT;
	} |
    hprint ;
hsemi:
    semi =
	{
		*ccharp++ = HPRINT;
	};
expr:
    e1 |
    monadic expr =
	{
		invert($$, $2);
	} |
    e1 dyadic expr =
	{
		invert($$, $3);
	} ;
e1:
    e2 |
    e2 lsub subs rbkt =
	{
		invert($$, $3);
		*ccharp++ = INDEX;
		*ccharp++ = scount;
		scount = $2;
	} ;
e2:
    nfun =
	{
		$$ = name($$, FUN);
	} |
    nam =
	{
		$$ = name($$, NAME);
	} |
    strng =
	{
		$$ = ccharp;
		ccharp += 2;
		integ = iline[-1];
		vcount = 0;
		for(;;) {
			if(*iline == '\n') {
				nlexsym = unk;
				break;
			}
			if(*iline == integ) {
				iline++;
				break;
			}
			*ccharp++ = *iline++;
			vcount++;
		}
		$$->c[0] = QUOT;
		$$->c[1] = vcount;
	} |
    vector =
	{
		*ccharp++ = CONST;
		*ccharp++ = vcount;
		invert($$, ccharp-2);
	} |
    lpar expr rpar =
	{
		$$ = $2;
	} |
    quad =
	{
		$$ = ccharp;
		*ccharp++ = $1;
	} ;
vector:
    number vector =
	{
		vcount++;
	} |
    number =
	{
		vcount = 1;
	} ;
number:
    numb =
	{
		$$ = ccharp;
		for(integ=0; integ<SDAT; integ++)
			*ccharp++ = datum.c[integ];
	} ;

/*
 * indexing subscripts
 * optional expressions separated by semi
 */
lsub:
    lbkt =
	{
		$$ = scount;
		scount = 1;
	} ;
subs:
    sub |
    subs semi sub =
	{
		invert($$, $3);
		scount++;
	} ;
sub:
    expr |
	=
	{
		$$ = ccharp;
		*ccharp++ = ELID;
	} ;

/*
 * return a string of a monadic operator.
 */
monadic:
    monad =
	{
		$$ = ccharp;
		*ccharp++ = $1;
	} |
    smonad subr =
	{
		$$ = $2;
		*ccharp++ = $1+1;
	} |
    mfun =
	{
		$$ = name($$, FUN);
	} |
    scalar comp =
	{
		$$ = ccharp;
		*ccharp++ = $2+1;
		*ccharp++ = $1;
	} |
    scalar com subr =
	{
		$$ = $3;
		*ccharp++ = $2+3;
		*ccharp++ = $1;
	} ;
monad:
    m |
    msub |
    mondya =
	{
		$$++;
	} ;
smonad:
    msub |
    mdsub =
	{
		$$ += 2;
	} ;

/*
 * return a string of a dyadic operator.
 */
dyadic:
    dyad =
	{
		$$ = ccharp;
		*ccharp++ = $1;
	} |
    sdyad subr =
	{
		$$ = $2;
		*ccharp++ = $1;
	} |
    dfun =
	{
		$$ = name($$, FUN);
	} |
    null dot scalar =
	{
		$$ = ccharp;
		*ccharp++ = OPROD;
		*ccharp++ = $3;
	} |
    scalar dot scalar =
	{
		$$ = ccharp;
		*ccharp++ = IPROD;
		*ccharp++ = $1;
		*ccharp++ = $3;
	} ;
sdyad:
    mdcom =
	{
		$$ += 2;
	} ;

/*
 * single expression subscript
 * as found on operators to select
 * a dimension.
 */
subr:
    lbkt expr rbkt =
	{
		$$ = $2;
	} ;

/*
 * various combinations
 */
comp:
    com | com0 ;
dyad:
    mondya | dscal | d | com0 | asg | com ;
mdcom:
    mdsub | com ;
mondya:
    mdscal | md | mdsub ;
scalar:
    mdscal | dscal ;
anyname:
    nam | nfun | mfun | dfun ;
%%
#include "tab.c"
#include "lex.c"
