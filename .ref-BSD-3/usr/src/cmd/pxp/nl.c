/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "opcode.h"

#ifdef PI
/*
 * Array of information about pre-defined, block 0 symbols.
 */
int	*biltins[] {

	/*
	 * Types
	 */
	"boolean",
	"char",
	"integer",
	"real",
	"_nil",		/* dummy name */
	0,

	/*
	 * Ranges
	 */
	TINT,		0177777, 0177600, 0, 0177,
	TINT,		0177777, 0100000, 0, 077777,
	TINT,		0100000, 0, 077777, 0177777,
	TCHAR,		0, 0, 0, 127,
	TBOOL,		0, 0, 0, 1,
	TDOUBLE,	0, 0, 0, 0,		/* fake for reals */
	0,

	/*
	 * Built-in composite types
	 */
	"Boolean",
	"intset",
	"alfa",
	"text",
	"input", 
	"output", 

	/*
	 * Built-in constants
	 */
	"true", 	TBOOL,	1, 0,
	"false", 	TBOOL,	0, 0,
	"minchar",	T1CHAR,	0, 0,
	"maxchar",	T1CHAR,	0177, 0,
	"bell",		T1CHAR,	07, 0,
	"tab",		T1CHAR,	011, 0,
	"minint",	T4INT,	0100000, 0,		/* Must be last 2! */
	"maxint",	T4INT,	077777, 0177777,
	0,

	/*
	 * Built-in functions
	 */
#ifndef PI0
	"abs",		O_ABS2,
	"arctan",	O_ATAN,
	"card",		O_CARD|NSTAND,
	"chr",		O_CHR2,
	"clock",	O_CLCK|NSTAND,
	"cos",		O_COS,
	"eof",		O_EOF,
	"eoln",		O_EOLN,
	"eos",		0,
	"exp",		O_EXP,
	"expo",		O_EXPO|NSTAND,
	"ln",		O_LN,
	"odd",		O_ODD2,
	"ord",		O_ORD2,
	"pred",		O_PRED2,
	"round",	O_ROUND,
	"sin",		O_SIN,
	"sqr",		O_SQR2,
	"sqrt",		O_SQRT,
	"succ",		O_SUCC2,
	"trunc",	O_TRUNC,
	"undefined",	O_UNDEF|NSTAND,
	/*
	 * Extensions
	 */
	"argc",		O_ARGC|NSTAND,
	"random",	O_RANDOM|NSTAND,
	"seed",		O_SEED|NSTAND,
	"wallclock",	O_WCLCK|NSTAND,
	"sysclock",	O_SCLCK|NSTAND,
	0,

	/*
	 * Built-in procedures
	 */
	"date",		O_DATE|NSTAND,
	"flush",	O_FLUSH|NSTAND,
	"get", 		O_GET,
	"getseg",	0,
	"halt",		O_HALT|NSTAND,
	"linelimit",	O_LLIMIT|NSTAND,
	"message",	O_MESSAGE|NSTAND,
	"new",		O_NEW,
	"pack",		O_PACK,
	"page",		O_PAGE,
	"put",		O_PUT,
	"putseg",	0,
	"read",		O_READ4,
	"readln",	O_READLN,
	"remove",	O_REMOVE|NSTAND,
	"reset",	O_RESET,
	"rewrite",	O_REWRITE,
	"time",		O_TIME|NSTAND,
	"unpack",	O_UNPACK,
	"write",	O_WRIT2,
	"writeln",	O_WRITLN,
	/*
	 * Extensions
	 */
	"argv",		O_ARGV|NSTAND,
	"null",		O_NULL|NSTAND,
	"stlimit",	O_STLIM|NSTAND,
	0,
#else
	"abs",
	"arctan",
	"card",
	"chr",
	"clock",
	"cos",
	"eof",
	"eoln",
	"eos",
	"exp",
	"expo",
	"ln",
	"odd",
	"ord",
	"pred",
	"round",
	"sin",
	"sqr",
	"sqrt",
	"succ",
	"trunc",
	"undefined",
	/*
	 * Extensions
	 */
	"argc",
	"random",
	"seed",
	"wallclock",
	"sysclock",
	0,

	/*
	 * Built-in procedures
	 */
	"date",
	"flush",
	"get",
	"getseg",
	"halt",
	"linelimit",
	"message",
	"new",
	"pack",
	"page",
	"put",
	"putseg",
	"read",
	"readln",
	"remove",
	"reset",
	"rewrite",
	"time",
	"unpack",
	"write",
	"writeln",
	/*
	 * Extensions
	 */
	"argv",
	"null",
	"stlimit",
	0,
#endif
};

/*
 * NAMELIST SEGMENT DEFINITIONS
 */
struct nls {
	struct nl *nls_low;
	struct nl *nls_high;
} ntab[MAXNL], *nlact;

struct	nl nl[INL];
struct	nl *nlp nl;
struct	nls *nlact ntab;
/*
 * Initnl initializes the first namelist segment and then
 * uses the array biltins to initialize the name list for
 * block 0.
 */
initnl()
{
	register int *q;
	register struct nl *p;
	register int i;

#ifdef DEBUG
	if (hp21mx) {
		MININT = -32768.;
		MAXINT = 32767.;
#ifndef PI0
		genmx();
#endif
	}
#endif
	ntab[0].nls_low = nl;
	ntab[0].nls_high = &nl[INL];
	defnl(0, 0, 0, 0);
	/*
	 * Fundamental types
	 */
	for (q = biltins; *q != 0; q++)
		hdefnl(*q, TYPE, nlp, 0);
	q++;

	/*
	 * Ranges
	 */
	while (*q) {
		p = defnl(0, RANGE, nl+*q, 0);
		nl[*q++].type = p;
		for (i = 0; i < 4; i++)
			p->value[i] = *q++;
	}
	q++;

#ifdef DEBUG
	if (hp21mx) {
		nl[T4INT].range[0] = MININT;
		nl[T4INT].range[1] = MAXINT;
	}
#endif

	/*
	 * Pre-defined composite types
	 */
	hdefnl(*q++, TYPE, nl+T1BOOL, 0);
	enter(defnl((intset = *q++), TYPE, nlp+1, 0));
	defnl(0, SET, nlp+1, 0);
	defnl(0, RANGE, nl+TINT, 0)->value[3] = 127;
     p=	defnl(0, RANGE, nl+TINT, 0);
	p->value[1] = 1;
	p->value[3] = 10;
	defnl(0, ARRAY, nl+T1CHAR, 1)->chain = p;
	hdefnl(*q++, TYPE, nlp-1, 0);	/* "alfa" */
	hdefnl(*q++, TYPE, nlp+1, 0);	/* "text" */
     p=	defnl(0, FILE, nl+T1CHAR, 0);
	p->nl_flags =| NFILES;
#ifndef PI0
	input = hdefnl(*q++, VAR, p, -2);	/* "input" */
	output = hdefnl(*q++, VAR, p, -4);	/* "output" */
#else
	input = hdefnl(*q++, VAR, p, 0);	/* "input" */
	output = hdefnl(*q++, VAR, p, 0);	/* "output" */
#endif

	/*
	 * Pre-defined constants
	 */
	for (; *q; q =+ 4)
		hdefnl(q[0], CONST, nl+q[1], q[2])->value[1] = q[3];

#ifdef DEBUG
	if (hp21mx) {
		nlp[-2].range[0] = MININT;
		nlp[-1].range[0] = MAXINT;
	}
#endif

	/*
	 * Built-in procedures and functions
	 */
#ifndef PI0
	for (q++; *q; q =+ 2)
		hdefnl(q[0], FUNC, 0, q[1]);
	for (q++; *q; q =+ 2)
		hdefnl(q[0], PROC, 0, q[1]);
#else
	for (q++; *q;)
		hdefnl(*q++, FUNC, 0, 0);
	for (q++; *q;)
		hdefnl(*q++, PROC, 0, 0);
#endif
}

hdefnl(sym, cls, typ, val)
{
	register struct nl *p;

#ifndef PI1
	if (sym)
		hash(sym, 0);
#endif
	p = defnl(sym, cls, typ, val);
	if (sym)
		enter(p);
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
		free(nlact->nls_low);
		nlact->nls_low = NIL;
		nlact->nls_high = NIL;
		--nlact;
		if (nlact < &ntab[0])
			panic("nlfree");
	}
}
#endif

char	VARIABLE[]	"variable";

char	*classes[] {
	"undefined",
	"constant",
	"type",
	VARIABLE,
	"array",
	"pointer or file",
	"record",
	"field",
	"procedure",
	"function",
	VARIABLE,
	VARIABLE,
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
#ifdef DEBUG
	"variant",
#endif
};

char	snark[]	"SNARK";

#ifdef PI
#ifdef DEBUG
char	*ctext[]
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
	"FILE",
	"SET",
	"RANGE",
	"LABEL",
	"WITHPTR",
	"SCAL",
	"STR",
	"PROG",
	"IMPROPER",
	"VARNT"
};

char	*stars	"\t***";

/*
 * Dump the namelist from the
 * current nlp down to 'to'.
 * All the namelist is dumped if
 * to is NIL.
 */
dumpnl(to, rout)
	struct nl *to;
{
	register struct nl *p;
	register int j;
	struct nls *nlsp;
	int i, v, head;

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
			putchar('\t');
			if (p->nl_flags & 037)
				printf("%d ", p->nl_flags & 037);
#ifndef PI0
			if (p->nl_flags & NMOD)
				putchar('M');
			if (p->nl_flags & NUSED)
				putchar('U');
#endif
			if (p->nl_flags & NFILES)
				putchar('F');
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
con:
						printf("\t%ld", p->range[0]);
						break;
					case TSTR:
						printf("\t'%s'", v);
						break;
					}
				break;
			case VAR:
			case REF:
			case WITHPTR:
				printf("\t%d,%d", cbn, v);
				break;
			case SCAL:
			case RANGE:
				printf("\t%ld..%ld", p->range[0], p->range[1]);
				break;
			case RECORD:
				printf("\t%d(%d)", v, p->value[NL_FLDSZ]);
				break;
			case FIELD:
				printf("\t%d", v);
				break;
			case STR:
				printf("\t\"%s\"", p->value[1]);
				goto casedef;
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
casedef:
				if (v)
					printf("\t<%d>", v);
				else
					printf(stars);
		}
		if (p->chain)
			printf("\t[%d]", nloff(p->chain));
		switch (p->class) {
			case RECORD:
				if (p->value[NL_VARNT])
					printf("\tVARNT=[%d]", nloff(p->value[NL_VARNT]));
				if (p->value[NL_TAG])
					printf(" TAG=[%d]", nloff(p->value[NL_TAG]));
				break;
			case VARNT:
				printf("\tVTOREC=[%d]", nloff(p->value[NL_VTOREC]));
				break;
		}
		putchar('\n');
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
	q = p;
	i = (sizeof *p)/2;
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
		cp = alloc(NLINC * sizeof *nlp);
		if (cp == -1) {
			i = NLINC / 2;
			cp = alloc((NLINC / 2) * sizeof *nlp);
		}
		if (cp == -1) {
			error("Ran out of memory (defnl)");
			pexit(DIED);
		}
		nlact++;
		if (nlact >= &ntab[MAXNL]) {
			error("Ran out of name list tables");
			pexit(DIED);
		}
		nlp = cp;
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
nlcopy(p)
	struct nl *p;
{
	register int *p1, *p2, i;

	p1 = p;
	p = p2 = defnl(0, 0, 0, 0);
	i = (sizeof *p)/2;
	do
		*p2++ = *p1++;
	while (--i);
	return (p);
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
	i = rp->symbol;
	i =& 077;
	hp = disptab[i];
	if (rp->class != BADUSE && rp->class != FIELD)
	for (p = hp; p != NIL && (p->nl_block & 037) == cbn; p = p->nl_next)
		if (p->symbol == rp->symbol && p->class != BADUSE && p->class != FIELD) {
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

double	MININT		-2147483648.;
double	MAXINT		2147483647.;
