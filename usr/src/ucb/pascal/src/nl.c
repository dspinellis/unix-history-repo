/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)nl.c	5.1 (Berkeley) 6/5/85";
#endif not lint


#include "whoami.h"
#include "0.h"
#ifdef PI
#include "opcode.h"
#include "objfmt.h"

/*
 * NAMELIST SEGMENT DEFINITIONS
 */
struct nls {
	struct nl *nls_low;
	struct nl *nls_high;
} ntab[MAXNL], *nlact;

struct	nl nl[INL];
struct	nl *nlp = nl;
struct	nls *nlact = ntab;

    /*
     *	all these strings must be places where people can find them
     *	since lookup only looks at the string pointer, not the chars.
     *	see, for example, pTreeInit.
     */

    /*
     *	built in constants
     */
char	*in_consts[] = {
	    "true" ,
	    "false" ,
	    "TRUE",
	    "FALSE",
	    "minint" ,
	    "maxint" ,
	    "minchar" ,
	    "maxchar" ,
	    "bell" ,
	    "tab" ,
	    0
	};

    /*
     *	built in simple types
     */
char *in_types[] = 
    {
	"boolean",
	"char",
	"integer",
	"real",
	"_nil",		/* dummy name */
	0
    };

int in_rclasses[] =
    {
	TINT , 
	TINT ,
	TINT ,
	TCHAR ,
	TBOOL ,
	TDOUBLE ,
	0
    };

long in_ranges[] =
    {
	-128L	 , 127L ,
	-32768L	 , 32767L ,
	-2147483648L , 2147483647L ,
	0L		 , 127L ,
	0L		 , 1L ,
	0L		 , 0L 		/* fake for reals */
    };

    /*
     *	built in constructed types
     */
char	*in_ctypes[] = {
	    "Boolean" ,
	    "intset" ,
	    "alfa" ,
	    "text" ,
	    0
	};

    /*
     *	built in variables
     */
char	*in_vars[] = {
	    "input" ,
	    "output" ,
	    0
	};

    /*
     *	built in functions 
     */
char *in_funcs[] =
    {
	"abs" ,
	"arctan" ,
	"card" ,
	"chr" ,
	"clock" ,
	"cos" ,
	"eof" ,
	"eoln" ,
	"eos" ,
	"exp" ,
	"expo" ,
	"ln" ,
	"odd" ,
	"ord" ,
	"pred" ,
	"round" ,
	"sin" ,
	"sqr" ,
	"sqrt" ,
	"succ" ,
	"trunc" ,
	"undefined" ,
	/*
	 * Extensions
	 */
	"argc" ,
	"random" ,
	"seed" ,
	"wallclock" ,
	"sysclock" ,
	0
    };

	/*
	 * Built-in procedures
	 */
char *in_procs[] =
    {
	"assert",
	"date" ,
	"dispose" ,
	"flush" ,
	"get" ,
	"getseg" ,
	"halt" ,
	"linelimit" ,
	"message" ,
	"new" ,
	"pack" ,
	"page" ,
	"put" ,
	"putseg" ,
	"read" ,
	"readln" ,
	"remove" ,
	"reset" ,
	"rewrite" ,
	"time" ,
	"unpack" ,
	"write" ,
	"writeln" ,
	/*
	 * Extensions
	 */
	"argv" ,
	"null" ,
	"stlimit" ,
	0
    };

#ifndef PI0
    /*
     *	and their opcodes
     */
int in_fops[] =
    {
	O_ABS2,
	O_ATAN,
	O_CARD|NSTAND,
	O_CHR2,
	O_CLCK|NSTAND,
	O_COS,
	O_EOF,
	O_EOLN,
	0,
	O_EXP,
	O_EXPO|NSTAND,
	O_LN,
	O_ODD2,
	O_ORD2,
	O_PRED2,
	O_ROUND,
	O_SIN,
	O_SQR2,
	O_SQRT,
	O_SUCC2,
	O_TRUNC,
	O_UNDEF|NSTAND,
	/*
	 * Extensions
	 */
	O_ARGC|NSTAND,
	O_RANDOM|NSTAND,
	O_SEED|NSTAND,
	O_WCLCK|NSTAND,
	O_SCLCK|NSTAND
    };

    /*
     * Built-in procedures
     */
int in_pops[] =
    {
	O_ASRT|NSTAND,
	O_DATE|NSTAND,
	O_DISPOSE,
	O_FLUSH|NSTAND,
	O_GET,
	0,
	O_HALT|NSTAND,
	O_LLIMIT|NSTAND,
	O_MESSAGE|NSTAND,
	O_NEW,
	O_PACK,
	O_PAGE,
	O_PUT,
	0,
	O_READ4,
	O_READLN,
	O_REMOVE|NSTAND,
	O_RESET,
	O_REWRITE,
	O_TIME|NSTAND,
	O_UNPACK,
	O_WRITEF,
	O_WRITLN,
	/*
	 * Extensions
	 */
	O_ARGV|NSTAND,
	O_ABORT|NSTAND,
	O_STLIM|NSTAND
    };
#endif

/*
 * Initnl initializes the first namelist segment and then
 * initializes the name list for block 0.
 */
initnl()
    {
	register char		**cp;
	register struct nl	*np;
	struct nl		*fp;
	int			*ip;
	long			*lp;

#ifdef	DEBUG
	if ( hp21mx )
	    {
		MININT = -32768.;
		MAXINT = 32767.;
#ifndef	PI0
#ifdef OBJ
		genmx();
#endif OBJ
#endif
	    }
#endif
	ntab[0].nls_low = nl;
	ntab[0].nls_high = &nl[INL];
	(void) defnl ( (char *) 0 , 0 , NLNIL , 0 );

	/*
	 *	Types
	 */
	for ( cp = in_types ; *cp != 0 ; cp ++ )
	    (void) hdefnl ( *cp , TYPE , nlp , 0 );

	/*
	 *	Ranges
	 */
	lp = in_ranges;
	for ( ip = in_rclasses ; *ip != 0 ; ip ++ )
	    {
		np = defnl ( (char *) 0 , RANGE , nl+(*ip) , 0 );
		nl[*ip].type = np;
		np -> range[0] = *lp ++ ;
		np -> range[1] = *lp ++ ;
	
	    };

	/*
	 *	built in constructed types
	 */
	
	cp = in_ctypes;
	/*
	 *	Boolean = boolean;
	 */
	(void) hdefnl ( *cp++ , TYPE , (struct nl *) (nl+T1BOOL) , 0 );

	/*
	 *	intset = set of 0 .. 127;
	 */
	intset = ((struct nl *) *cp++);
	(void) hdefnl( (char *) intset , TYPE , nlp+1 , 0 );
	(void) defnl ( (char *) 0 , SET , nlp+1 , 0 );
	np = defnl ( (char *) 0 , RANGE , nl+TINT , 0 );
	np -> range[0] = 0L;
	np -> range[1] = 127L;

	/*
	 *	alfa = array [ 1 .. 10 ] of char;
	 */
	np = defnl ( (char *) 0 , RANGE , nl+TINT , 0 );
	np -> range[0] = 1L;
	np -> range[1] = 10L;
	defnl ( (char *) 0 , ARRAY , nl+T1CHAR , 1 ) -> chain = np;
	(void) hdefnl ( *cp++ , TYPE , nlp-1 , 0 );

	/*
	 *	text = file of char;
	 */
	(void) hdefnl ( *cp++ , TYPE , nlp+1 , 0 );
	np = defnl ( (char *) 0 , FILET , nl+T1CHAR , 0 );
	np -> nl_flags |= NFILES;

	/*
	 *	input,output : text;
	 */
	cp = in_vars;
#	ifndef	PI0
		input = hdefnl ( *cp++ , VAR , np , INPUT_OFF );
		output = hdefnl (  *cp++ , VAR , np , OUTPUT_OFF );
#	else
		input = hdefnl ( *cp++ , VAR , np , 0 );
		output = hdefnl ( *cp++ , VAR , np , 0 );
#	endif
#	ifdef PC
	    input -> extra_flags |= NGLOBAL;
	    output -> extra_flags |= NGLOBAL;
#	endif PC

	/*
	 *	built in constants
	 */
	cp = in_consts;
	np = hdefnl ( *cp++ , CONST , nl + TBOOL , 1 );
	fp = hdefnl ( *cp++ , CONST , nl + TBOOL , 0 );
	(nl + TBOOL)->chain = fp;
	fp->chain = np;
	np = hdefnl ( *cp++ , CONST , nl + TBOOL , 1 );
	fp = hdefnl ( *cp++ , CONST , nl + TBOOL , 0 );
	fp->chain = np;
	if (opt('s'))
		(nl + TBOOL)->chain = fp;
	hdefnl ( *cp++ , CONST , nl + T4INT , 0 ) -> range[0] = MININT;
	hdefnl ( *cp++ , CONST , nl + T4INT , 0 ) -> range[0] = MAXINT;
	(void) hdefnl ( *cp++ , CONST , nl + T1CHAR , 0 );
	(void) hdefnl ( *cp++ , CONST , nl + T1CHAR , 127 );
	(void) hdefnl ( *cp++ , CONST , nl + T1CHAR , '\007' );
	(void) hdefnl ( *cp++ , CONST , nl + T1CHAR , '\t' );

	/*
	 * Built-in functions and procedures
	 */
#ifndef PI0
	ip = in_fops;
	for ( cp = in_funcs ; *cp != 0 ; cp ++ )
	    (void) hdefnl ( *cp , FUNC , NLNIL , * ip ++ );
	ip = in_pops;
	for ( cp = in_procs ; *cp != 0 ; cp ++ )
	    (void) hdefnl ( *cp , PROC , NLNIL , * ip ++ );
#else
	for ( cp = in_funcs ; *cp != 0 ; cp ++ )
	    (void) hdefnl ( *cp , FUNC , NLNIL , 0 );
	for ( cp = in_procs ; *cp != 0 , cp ++ )
	    (void) hdefnl ( *cp , PROC , NLNIL , 0 );
#endif
#	ifdef PTREE
	    pTreeInit();
#	endif
    }

struct nl *
hdefnl(sym, cls, typ, val)
    char *sym;
    int  cls;
    struct nl *typ;
    int val;
{
	register struct nl *p;

#ifndef PI1
	if (sym)
		(void) hash(sym, 0);
#endif
	p = defnl(sym, cls, typ, val);
	if (sym)
		(void) enter(p);
	return (p);
}

/*
 * Free up the name list segments
 * at the end of a statement/proc/func
 * All segments are freed down to the one in which
 * p points.
 */
nlfree(p)
	struct nl *p;
{

	nlp = p;
	while (nlact->nls_low > nlp || nlact->nls_high < nlp) {
		free((char *) nlact->nls_low);
		nlact->nls_low = NIL;
		nlact->nls_high = NIL;
		--nlact;
		if (nlact < &ntab[0])
			panic("nlfree");
	}
}
#endif PI


#ifndef PC
#ifndef OBJ
char	*VARIABLE	= "variable";
#endif PC
#endif OBJ

char	*classes[ ] = {
	"undefined",
	"constant",
	"type",
	"variable",	/*	VARIABLE	*/
	"array",
	"pointer or file",
	"record",
	"field",
	"procedure",
	"function",
	"variable",	/*	VARIABLE	*/
	"variable",	/*	VARIABLE	*/
	"pointer",
	"file",
	"set",
	"subrange",
	"label",
	"withptr",
	"scalar",
	"string",
	"program",
	"improper",
	"variant",
	"formal procedure",
	"formal function"
};

#ifndef PC
#ifndef OBJ
char	*snark	= "SNARK";
#endif
#endif

#ifdef PI
#ifdef DEBUG
char	*ctext[] =
{
	"BADUSE",
	"CONST",
	"TYPE",
	"VAR",
	"ARRAY",
	"PTRFILE",
	"RECORD",
	"FIELD",
	"PROC",
	"FUNC",
	"FVAR",
	"REF",
	"PTR",
	"FILET",
	"SET",
	"RANGE",
	"LABEL",
	"WITHPTR",
	"SCAL",
	"STR",
	"PROG",
	"IMPROPER",
	"VARNT",
	"FPROC",
	"FFUNC",
	"CRANGE"
};

char	*stars	= "\t***";

/*
 * Dump the namelist from the
 * current nlp down to 'to'.
 * All the namelist is dumped if
 * to is NIL.
 */
/*VARARGS*/
dumpnl(to, rout)
	struct nl *to;
{
	register struct nl *p;
	struct nls *nlsp;
	int v, head;

	if (opt('y') == 0)
		return;
	if (to != NIL)
		printf("\n\"%s\" Block=%d\n", rout, cbn);
	nlsp = nlact;
	head = NIL;
	for (p = nlp; p != to;) {
		if (p == nlsp->nls_low) {
			if (nlsp == &ntab[0])
				break;
			nlsp--;
			p = nlsp->nls_high;
		}
		p--;
		if (head == NIL) {
			printf("\tName\tClass  Bn+Flags\tType\tVal\tChn\n");
			head++;
		}
		printf("%3d:", nloff(p));
		if (p->symbol)
			printf("\t%.7s", p->symbol);
		else
			printf(stars);
		if (p->class)
			printf("\t%s", ctext[p->class]);
		else
			printf(stars);
		if (p->nl_flags) {
			pchr('\t');
			if (p->nl_flags & 037)
				printf("%d ", p->nl_flags & 037);
#ifndef PI0
			if (p->nl_flags & NMOD)
				pchr('M');
			if (p->nl_flags & NUSED)
				pchr('U');
#endif
			if (p->nl_flags & NFILES)
				pchr('F');
		} else
			printf(stars);
		if (p->type)
			printf("\t[%d]", nloff(p->type));
		else
			printf(stars);
		v = p->value[0];
		switch (p->class) {
			case TYPE:
				break;
			case VARNT:
				goto con;
			case CONST:
				switch (nloff(p->type)) {
					default:
						printf("\t%d", v);
						break;
					case TDOUBLE:
						printf("\t%f", p->real);
						break;
					case TINT:
					case T4INT:
con:
						printf("\t%ld", p->range[0]);
						break;
					case TSTR:
						printf("\t'%s'", p->ptr[0]);
						break;
					}
				break;
			case VAR:
			case REF:
			case WITHPTR:
			case FFUNC:
			case FPROC:
				printf("\t%d,%d", cbn, v);
				break;
			case SCAL:
			case RANGE:
				printf("\t%ld..%ld", p->range[0], p->range[1]);
				break;
			case CRANGE:
				printf("\t%s..%s", p->nptr[0]->symbol,
					p->nptr[1]->symbol);
				break;
			case RECORD:
				printf("\t%d", v);
				break;
			case FIELD:
				printf("\t%d", v);
				break;
			case STR:
				printf("\t|%d|", p->value[0]);
				break;
			case FVAR:
			case FUNC:
			case PROC:
			case PROG:
				if (cbn == 0) {
					printf("\t<%o>", p->value[0] & 0377);
#ifndef PI0
					if (p->value[0] & NSTAND)
						printf("\tNSTAND");
#endif
					break;
				}
				v = p->value[1];
			default:

				if (v)
					printf("\t<%d>", v);
				else
					printf(stars);
		}
		if (p->chain)
			printf("\t[%d]", nloff(p->chain));
		switch (p->class) {
			case RECORD:
				printf("\tALIGN=%d", p->align_info);
				if (p->ptr[NL_FIELDLIST]) {
				    printf(" FLIST=[%d]",
					nloff(p->ptr[NL_FIELDLIST]));
				} else {
				    printf(" FLIST=[]");
				}
				if (p->ptr[NL_TAG]) {
				    printf(" TAG=[%d]",
					nloff(p->ptr[NL_TAG]));
				} else {
				    printf(" TAG=[]");
				}
				if (p->ptr[NL_VARNT]) {
				    printf(" VARNT=[%d]",
					nloff(p->ptr[NL_VARNT]));
				} else {
				    printf(" VARNT=[]");
				}
				break;
			case FIELD:
				if (p->ptr[NL_FIELDLIST]) {
				    printf("\tFLIST=[%d]",
					nloff(p->ptr[NL_FIELDLIST]));
				} else {
				    printf("\tFLIST=[]");
				}
				break;
			case VARNT:
				printf("\tVTOREC=[%d]",
				    nloff(p->ptr[NL_VTOREC]));
				break;
		}
#		ifdef PC
		    if ( p -> extra_flags != 0 ) {
			pchr( '\t' );
			if ( p -> extra_flags & NEXTERN )
			    printf( "NEXTERN " );
			if ( p -> extra_flags & NLOCAL )
			    printf( "NLOCAL " );
			if ( p -> extra_flags & NPARAM )
			    printf( "NPARAM " );
			if ( p -> extra_flags & NGLOBAL )
			    printf( "NGLOBAL " );
			if ( p -> extra_flags & NREGVAR )
			    printf( "NREGVAR " );
		    }
#		endif PC
#		ifdef PTREE
		    pchr( '\t' );
		    pPrintPointer( stdout , "%s" , p -> inTree );
#		endif
		pchr('\n');
	}
	if (head == 0)
		printf("\tNo entries\n");
}
#endif


/*
 * Define a new name list entry
 * with initial symbol, class, type
 * and value[0] as given.  A new name
 * list segment is allocated to hold
 * the next name list slot if necessary.
 */
struct nl *
defnl(sym, cls, typ, val)
	char *sym;
	int cls;
	struct nl *typ;
	int val;
{
	register struct nl *p;
	register int *q, i;
	char *cp;

	p = nlp;

	/*
	 * Zero out this entry
	 */
	q = ((int *) p);
	i = (sizeof *p)/(sizeof (int));
	do
		*q++ = 0;
	while (--i);

	/*
	 * Insert the values
	 */
	p->symbol = sym;
	p->class = cls;
	p->type = typ;
	p->nl_block = cbn;
	p->value[0] = val;

	/*
	 * Insure that the next namelist
	 * entry actually exists. This is
	 * really not needed here, it would
	 * suffice to do it at entry if we
	 * need the slot.  It is done this
	 * way because, historically, nlp
	 * always pointed at the next namelist
	 * slot.
	 */
	nlp++;
	if (nlp >= nlact->nls_high) {
		i = NLINC;
		cp = (char *) malloc(NLINC * sizeof *nlp);
		if (cp == 0) {
			i = NLINC / 2;
			cp = (char *) malloc((NLINC / 2) * sizeof *nlp);
		}
		if (cp == 0) {
			error("Ran out of memory (defnl)");
			pexit(DIED);
		}
		nlact++;
		if (nlact >= &ntab[MAXNL]) {
			error("Ran out of name list tables");
			pexit(DIED);
		}
		nlp = (struct nl *) cp;
		nlact->nls_low = nlp;
		nlact->nls_high = nlact->nls_low + i;
	}
	return (p);
}

/*
 * Make a duplicate of the argument
 * namelist entry for, e.g., type
 * declarations of the form 'type a = b'
 * and array indicies.
 */
struct nl *
nlcopy(p)
	struct nl *p;
{
	register struct nl *p1, *p2;

	p1 = p;
	p2 = defnl((char *) 0, 0, NLNIL, 0);
	*p2 = *p1;
	p2->chain = NLNIL;
	return (p2);
}

/*
 * Compute a namelist offset
 */
nloff(p)
	struct nl *p;
{

	return (p - nl);
}

/*
 * Enter a symbol into the block
 * symbol table.  Symbols are hashed
 * 64 ways based on low 6 bits of the
 * character pointer into the string
 * table.
 */
struct nl *
enter(np)
	struct nl *np;
{
	register struct nl *rp, *hp;
	register struct nl *p;
	int i;

	rp = np;
	if (rp == NIL)
		return (NIL);
#ifndef PI1
	if (cbn > 0)
		if (rp->symbol == input->symbol || rp->symbol == output->symbol)
			error("Pre-defined files input and output must not be redefined");
#endif
	i = (int) rp->symbol;
	i &= 077;
	hp = disptab[i];
	if (rp->class != BADUSE && rp->class != FIELD)
	for (p = hp; p != NIL && (p->nl_block & 037) == cbn; p = p->nl_next)
		if (p->symbol == rp->symbol && p->symbol != NIL &&
		    p->class != BADUSE && p->class != FIELD) {
#ifndef PI1
			error("%s is already defined in this block", rp->symbol);
#endif
			break;

		}
	rp->nl_next = hp;
	disptab[i] = rp;
	return (rp);
}
#endif
