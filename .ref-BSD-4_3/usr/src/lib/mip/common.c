/*	common.c	4.2	85/08/22	*/

#ifdef PASS1COMMON
#include "pass1.h"
#else
#ifdef PASS2COMMON
#include "pass2.h"
#endif
#endif

#ifdef FORT
#undef BUFSTDERR
#endif
#ifndef ONEPASS
#undef BUFSTDERR
#endif
# ifndef EXIT
# define EXIT exit
# endif

int nerrors = 0;  /* number of errors */

extern unsigned int offsz;

unsigned caloff(){
	register i;
	unsigned int temp;
	unsigned int off;
	temp = 1;
	i = 0;
	do {
		temp <<= 1;
		++i;
		} while( temp != 0 );
	off = 1 << (i-1);
	return (off);
	}

NODE *lastfree;  /* pointer to last free node; (for allocator) */

	/* VARARGS1 */
uerror( s, a ) char *s; { /* nonfatal error message */
	/* the routine where is different for pass 1 and pass 2;
	/*  it tells where the error took place */

	++nerrors;
	where('u');
	fprintf( stderr, s, a );
	fprintf( stderr, "\n" );
#ifdef BUFSTDERR
	fflush(stderr);
#endif
	if( nerrors > 30 ) cerror( "too many errors");
	}

	/* VARARGS1 */
cerror( s, a, b, c ) char *s; { /* compiler error: die */
	where('c');
	if( nerrors && nerrors <= 30 ){ /* give the compiler the benefit of the doubt */
		fprintf( stderr, "cannot recover from earlier errors: goodbye!\n" );
		}
	else {
		fprintf( stderr, "compiler error: " );
		fprintf( stderr, s, a, b, c );
		fprintf( stderr, "\n" );
		}
#ifdef BUFSTDERR
	fflush(stderr);
#endif
	EXIT(1);
	}

int Wflag = 0; /* Non-zero means do not print warnings */

	/* VARARGS1 */
werror( s, a, b ) char *s; {  /* warning */
	if(Wflag) return;
	where('w');
	fprintf( stderr, "warning: " );
	fprintf( stderr, s, a, b );
	fprintf( stderr, "\n" );
#ifdef BUFSTDERR
	fflush(stderr);
#endif
	}

tinit(){ /* initialize expression tree search */

	register NODE *p;

	for( p=node; p<= &node[TREESZ-1]; ++p ) p->in.op = FREE;
	lastfree = node;

	}

# define TNEXT(p) (p== &node[TREESZ-1]?node:p+1)

NODE *
talloc(){
	register NODE *p, *q;

	q = lastfree;
	for( p = TNEXT(q); p!=q; p= TNEXT(p))
		if( p->in.op ==FREE ) return(lastfree=p);

	cerror( "out of tree space; simplify expression");
	/* NOTREACHED */
	}

tcheck(){ /* ensure that all nodes have been freed */

	register NODE *p;

	if( !nerrors )
		for( p=node; p<= &node[TREESZ-1]; ++p )
			if( p->in.op != FREE ) cerror( "wasted space: %o", p );
	tinit();
#ifdef FLEXNAMES
	freetstr();
#endif
	}
tfree( p )  NODE *p; {
	/* free the tree p */
	extern tfree1();

	if( p->in.op != FREE ) walkf( p, tfree1 );

	}

tfree1(p)  NODE *p; {
	if( p == 0 ) cerror( "freeing blank tree!");
	else p->in.op = FREE;
	}

fwalk( t, f, down ) register NODE *t; int (*f)(); {

	int down1, down2;

	more:
	down1 = down2 = 0;

	(*f)( t, down, &down1, &down2 );

	switch( optype( t->in.op ) ){

	case BITYPE:
		fwalk( t->in.left, f, down1 );
		t = t->in.right;
		down = down2;
		goto more;

	case UTYPE:
		t = t->in.left;
		down = down1;
		goto more;

		}
	}

#ifndef vax
walkf( t, f ) register NODE *t;  int (*f)(); {
	register opty;

	opty = optype(t->in.op);

	if( opty != LTYPE ) walkf( t->in.left, f );
	if( opty == BITYPE ) walkf( t->in.right, f );
	(*f)( t );
	}
#else
#define	NR	100

/*
 * Deliberately avoids recursion -- use this version on machines with
 * expensive procedure calls.
 */
walkf(t, f)
	register NODE *t;
	register int (*f)();
{
	register int i = 1;
	register int opty = optype(t->in.op);
	static NODE *at[NR];
	static int ao[NR];

#define	PUSH(dir, state) \
	(ao[i] = state, at[i++] = t, t = t->in.dir, opty = optype(t->in.op))
#define	POP() \
	(opty = ao[--i], t = at[i])

	do {
		switch (opty) {
		case LTYPE:	(*f)(t); POP(); break;
		case UTYPE:	PUSH(left, LTYPE); break;
		case BITYPE:	PUSH(left, BITYPE+1); break;
		case BITYPE+1:	PUSH(right, LTYPE); break;
		default:
			cerror("bad op type in walkf");
		}
		if (i >= NR) {
			walkf(t, f);
			POP();
		}
	} while (i > 0);
}
#undef NR
#undef PUSH
#undef POP
#endif



int dope[ DSIZE ];
char *opst[DSIZE];

struct dopest { int dopeop; char opst[8]; int dopeval; } indope[] = {

	NAME, "NAME", LTYPE,
	STRING, "STRING", LTYPE,
	REG, "REG", LTYPE,
	OREG, "OREG", LTYPE,
	ICON, "ICON", LTYPE,
	FCON, "FCON", LTYPE,
	DCON, "DCON", LTYPE,
	CCODES, "CCODES", LTYPE,
	UNARY MINUS, "U-", UTYPE,
	UNARY MUL, "U*", UTYPE,
	UNARY AND, "U&", UTYPE,
	UNARY CALL, "UCALL", UTYPE|CALLFLG,
	UNARY FORTCALL, "UFCALL", UTYPE|CALLFLG,
	NOT, "!", UTYPE|LOGFLG,
	COMPL, "~", UTYPE,
	FORCE, "FORCE", UTYPE,
	INIT, "INIT", UTYPE,
	SCONV, "SCONV", UTYPE,
	PCONV, "PCONV", UTYPE,
	PLUS, "+", BITYPE|FLOFLG|SIMPFLG|COMMFLG,
	ASG PLUS, "+=", BITYPE|ASGFLG|ASGOPFLG|FLOFLG|SIMPFLG|COMMFLG,
	MINUS, "-", BITYPE|FLOFLG|SIMPFLG,
	ASG MINUS, "-=", BITYPE|FLOFLG|SIMPFLG|ASGFLG|ASGOPFLG,
	MUL, "*", BITYPE|FLOFLG|MULFLG,
	ASG MUL, "*=", BITYPE|FLOFLG|MULFLG|ASGFLG|ASGOPFLG,
	AND, "&", BITYPE|SIMPFLG|COMMFLG,
	ASG AND, "&=", BITYPE|SIMPFLG|COMMFLG|ASGFLG|ASGOPFLG,
	QUEST, "?", BITYPE,
	COLON, ":", BITYPE,
	ANDAND, "&&", BITYPE|LOGFLG,
	OROR, "||", BITYPE|LOGFLG,
	CM, ",", BITYPE,
	COMOP, ",OP", BITYPE,
	ASSIGN, "=", BITYPE|ASGFLG,
	DIV, "/", BITYPE|FLOFLG|MULFLG|DIVFLG,
	ASG DIV, "/=", BITYPE|FLOFLG|MULFLG|DIVFLG|ASGFLG|ASGOPFLG,
	MOD, "%", BITYPE|DIVFLG,
	ASG MOD, "%=", BITYPE|DIVFLG|ASGFLG|ASGOPFLG,
	LS, "<<", BITYPE|SHFFLG,
	ASG LS, "<<=", BITYPE|SHFFLG|ASGFLG|ASGOPFLG,
	RS, ">>", BITYPE|SHFFLG,
	ASG RS, ">>=", BITYPE|SHFFLG|ASGFLG|ASGOPFLG,
	OR, "|", BITYPE|COMMFLG|SIMPFLG,
	ASG OR, "|=", BITYPE|COMMFLG|SIMPFLG|ASGFLG|ASGOPFLG,
	ER, "^", BITYPE|COMMFLG|SIMPFLG,
	ASG ER, "^=", BITYPE|COMMFLG|SIMPFLG|ASGFLG|ASGOPFLG,
	INCR, "++", BITYPE|ASGFLG,
	DECR, "--", BITYPE|ASGFLG,
	STREF, "->", BITYPE,
	CALL, "CALL", BITYPE|CALLFLG,
	FORTCALL, "FCALL", BITYPE|CALLFLG,
	EQ, "==", BITYPE|LOGFLG,
	NE, "!=", BITYPE|LOGFLG,
	LE, "<=", BITYPE|LOGFLG,
	LT, "<", BITYPE|LOGFLG,
	GE, ">", BITYPE|LOGFLG,
	GT, ">", BITYPE|LOGFLG,
	UGT, "UGT", BITYPE|LOGFLG,
	UGE, "UGE", BITYPE|LOGFLG,
	ULT, "ULT", BITYPE|LOGFLG,
	ULE, "ULE", BITYPE|LOGFLG,
#ifdef ARS
	ARS, "A>>", BITYPE,
#endif
	TYPE, "TYPE", LTYPE,
	LB, "[", BITYPE,
	CBRANCH, "CBRANCH", BITYPE,
	FLD, "FLD", UTYPE,
	PMCONV, "PMCONV", BITYPE,
	PVCONV, "PVCONV", BITYPE,
	RETURN, "RETURN", BITYPE|ASGFLG|ASGOPFLG,
	CAST, "CAST", BITYPE|ASGFLG|ASGOPFLG,
	GOTO, "GOTO", UTYPE,
	STASG, "STASG", BITYPE|ASGFLG,
	STARG, "STARG", UTYPE,
	STCALL, "STCALL", BITYPE|CALLFLG,
	UNARY STCALL, "USTCALL", UTYPE|CALLFLG,

	-1,	"",	0
};

mkdope(){
	register struct dopest *q;

	for( q = indope; q->dopeop >= 0; ++q ){
		dope[q->dopeop] = q->dopeval;
		opst[q->dopeop] = q->opst;
		}
	}
# ifndef BUG4
tprint( t )  TWORD t; { /* output a nice description of the type of t */

	static char * tnames[] = {
		"undef",
		"farg",
		"char",
		"short",
		"int",
		"long",
		"float",
		"double",
		"strty",
		"unionty",
		"enumty",
		"moety",
		"uchar",
		"ushort",
		"unsigned",
		"ulong",
		"?", "?"
		};

	for(;; t = DECREF(t) ){

		if( ISPTR(t) ) printf( "PTR " );
		else if( ISFTN(t) ) printf( "FTN " );
		else if( ISARY(t) ) printf( "ARY " );
		else {
			printf( "%s", tnames[t] );
			return;
			}
		}
	}
# endif

#ifdef FLEXNAMES
#define	NTSTRBUF	40
#define	TSTRSZ		2048
char	itstrbuf[TSTRSZ];
char	*tstrbuf[NTSTRBUF] = { itstrbuf };
char	**curtstr = tstrbuf;
int	tstrused;

char *
tstr(cp)
	register char *cp;
{
	register int i = strlen(cp);
	register char *dp;
	
	if (tstrused + i >= TSTRSZ) {
		if (++curtstr >= &tstrbuf[NTSTRBUF])
			cerror("out of temporary string space");
		tstrused = 0;
		if (*curtstr == 0) {
			dp = (char *)malloc(TSTRSZ);
			if (dp == 0)
				cerror("out of memory (tstr)");
			*curtstr = dp;
		}
	}
	strcpy(dp = *curtstr+tstrused, cp);
	tstrused += i + 1;
	return (dp);
}
#endif
