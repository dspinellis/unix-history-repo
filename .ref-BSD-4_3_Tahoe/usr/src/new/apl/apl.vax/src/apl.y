%{
static char apl_y_Sccsid[] = "apl.y @(#)apl.y	1.3	10/5/82 Berkeley ";
%}
%union {
	char	*charptr;
	char	charval;
}
%term   lex0, lex1, lex2, lex3, lex4, lex5, lex6
%term lpar, rpar, lbkt, rbkt, eol, unk
%term <charval> com, com0, Quad, asg
%term null, dot, cln, semi, comnt, tran
%term <charptr> strng nam, numb, nfun, mfun, dfun
%term <charval> comexpr, comnam, comnull, comlist

%term <charval> dscal, mdscal
%term <charval> m, d, md, msub, mdsub

%type <charptr> func, header, args, autos, labels, label
%type <charptr> fstat0, stat, statement, output, expr
%type <charptr> e1, e2, number, subs, sub, monadic
%type <charptr> dyadic, subr, anyname, hprint
%type <charval> comand, lsub, monad, smonad, sdyad
%type <charval> comp, dyad, mdcom, mondya, scalar

%{
#include "apl.h"
	int	vcount;
	int	scount;
	int	litflag;
	int	nlexsym;
	int	context;
	char	*iline;
	char	*ccharp, *ccharp2;
	data	lnumb;		/* current label number */
	char	*labcpp;	/* label prologue */
	char	*labcpe;	/* label epilogue */
	int	immedcmd;	/* immediate command number */
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
		if(integ != ASGN && integ != PRINT && integ != COMNT)
			*ccharp++ = PRINT;
		*ccharp++ = EOL;
	} |
    lex0 bcomand comand eol =
	{
		*ccharp++ = IMMED;
		*ccharp++ = $3;
	} |
/*
 * immediate mode state indicator stuff
 */
    lex0 tran eol =
	{
		*ccharp++ = SICLR0;
	} |
    lex0 tran expr eol =
	{
		*ccharp++ = SICLR;
	} |
/*
 * quad input
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
			/*
			 * see comments in ai.c/funcomp() concerning
			 * label processing.
			 */
			*ccharp++ = ELID;
			break;

		case lex4:
			ccharp2 = ccharp;
			*ccharp++ = EOL;
			name($$, RVAL);
			name($$, REST);
			invert($3, ccharp2);
		}
	} |
    header =
	{
		if(context == lex3)
			*ccharp++ = ELID;
		if(context == lex4){
			*ccharp++ = EOL;	/* pop previous result */
			*ccharp++ = NILRET;	/* return empty result */
		}
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
			name($3, ARG2);
			name($1, ARG1);
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
			ccharp2 = name($2, REST);
			invert($$, ccharp2);
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
    comlist anylist |
    comnull ;

anylist:
    anylist anyname =
	{
	    *ccharp++ = IMMED;
	    *ccharp++ = immedcmd;
	    name($2, NAME);
	} |
    anyname =
	{
	    name($1, NAME);
	};


/*
 * statement:
 *	comments
 *	expressions
 *	heterogeneous output
 *	transfers (in functions)
 */
fstat:
	labels fstat0 | fstat0;

labels:
	label | labels label;

label:
	anyname cln = {
		if(labgen)
			genlab($1);
	}  ;

fstat0:
    stat =
	{
		integ = ccharp[-1];
		if(integ != ASGN && integ != PRINT && integ != COMNT)
			*ccharp++ = PRINT;
	} |
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
    eol =
	{
		$$ = ccharp;
		*ccharp++ = COMNT;
	} |
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
				if(*iline != integ)
					break;
			}
			*ccharp++ = *iline++;
			vcount++;
		}
		((struct chrstrct *)$$)->c[0] = QUOT;
		((struct chrstrct *)$$)->c[1] = vcount;
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
    Quad =
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
		ccharp += copy(DA,&datum,ccharp,1);
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
