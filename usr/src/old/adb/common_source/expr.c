#ifndef lint
static char sccsid[] = "@(#)expr.c	5.1 (Berkeley) %G%";
#endif

/*
 * adb - expression parser
 */

#include "defs.h"
#include <ctype.h>

extern char BADSYM[];		/* "symbol not found" */
extern char BADVAR[];		/* "bad variable" */
extern char BADSYN[];		/* "syntax error" */
extern char NOCFN[];		/* "c routine not found" */
extern char NOADR[];		/* "address expected" */
extern char BADLOC[];		/* "automatic variable not found" */
extern char NOPCS[];		/* "no process" */

struct	nlist *xxxsym;		/* last symbol found due to expression */
	/* change this name back to cursym AFTER testing!... */
struct	activation curframe;	/* current stack frame (for local vars) */

/* 
 * This file implements a small recursive descent expression parser.
 * The syntax is (in YACC terms):
 *
 *	expr	: expr1
 *		|		(empty)
 *		;
 *
 *	expr1	: term
 *		| term dyadic expr1
 *		;
 *
 *	dyadic	: '+'		(addition)
 *		| '-'		(subtraction)
 *		| '#'		(roundup)
 *		| '*'		(multiplication)
 *		| '%'		(division)
 *		| '&'		(bitwise and)
 *		| '|'		(bitwise or)
 *		;
 *
 *	term	: item
 *		| monadic term
 *		| '(' expr ')'
 *		;
 *
 *	monadic	: '*'		(contents of core, or SP_DATA)
 *		| '@'		(contents of a.out, or SP_INSTR)
 *		| '-'		(negation)
 *		| '~'		(bitwise not)
 *		| '#'		(logical not)
 *		;
 *
 *	item	: number	(current radix; 0o,0t,0x; or float)
 *		| name		(value from symbol table)
 *		| rtn '.' name	(address of name in routine rtn)
 *		| rtn '.'	(???)
 *		| '.' name	(???)
 *		| '.'		(value of dot)
 *		| '+'		(dot + current increment)
 *		| '^'		(dot - current increment)
 *		| '"'		(last address typed)
 *		| '<' var	(value of variable var)
 *		| '<' register	(value in register)
 *		| '\'' ch '\''	(character(s))
 *		;
 *
 * The empty string handling is actually done in `item', but callers
 * can simply assume that expr() returns 1 if it finds an expression,
 * or 0 if not, and that rexpr() errors out if there is no expression.
 *
 * The routines symchar() and getsym() handle `name's and `rtn's.
 * The routine getnum(), with helper getfloat(), handles `number's.
 */

/* flags for symchar() */
#define	SYMCH_READ	1	/* call readchar() first */
#define	SYMCH_DIGITS	2	/* allow digits */

/*
 * Return true if the next (how & SYMCH_READ) or current character
 * is a symbol character; allow digits if (how & SYMCH_DIGITS).
 */
static int
symchar(how)
	int how;
{

	if (how & SYMCH_READ)
		(void) readchar();
	if (lastc == '\\') {
		(void) readchar();
		return (1);
	}
	if (isalpha(lastc) || lastc == '_')
		return (1);
	return ((how & SYMCH_DIGITS) && isdigit(lastc));
}

/*
 * Read a symbol into the given buffer.  The first character is
 * assumed already to have been read.
 */
static
getsym(symbuf, symlen)
	register char *symbuf;
	register int symlen;
{

	do {
		if (--symlen > 0)
			*symbuf++ = lastc;
	} while (symchar(SYMCH_READ | SYMCH_DIGITS));
	*symbuf = 0;
}

/*
 * Read a number.  The converted value is stored in expv.
 * The caller has already determined that there is at least one digit.
 */
static
getnum()
{
	register int base, c;

	expv = 0;
	if ((base = radix) < 0)
		base = -base;
	if (lastc == '0') {
		switch (readchar()) {
		case 'x': case 'X':
			base = 16;
			(void) readchar();
			break;
		case 't': case 'T':
			base = 10;
			(void) readchar();
			break;
		case 'o': case 'O':
			base = 8;
			(void) readchar();
		}
	}
	for (c = lastc; isxdigit(c); c = readchar()) {
		if (isdigit(c))
			c -= '0';
		else if (base <= 10)
			break;
		else
			c -= isupper(c) ? 'A' - 10 : 'a' - 10;
		if (c >= base)
			error(BADSYN);
		/* since expv is unsigned, the following cannot overflow */
		expv = expv * base + c;
	}
	if (lastc == '.' && (base == 10 || expv == 0))
		getfloat();
	unreadc();
}

/*
 * Read a float.  The integer part is already in expv.  Set expv
 * to the integer bit pattern that corresponds to the float.
 *
 * The following routine could be improved, but at least it will
 * not crash on input such as 0.999999999999999999999999999999,
 * as did the original.
 */
getfloat()
{
	register int i;
	register char *p;
 /* THE FOLLOWING ASSUMES sizeof(float)==sizeof(expr_t) */
 /* PERHAPS THIS SHOULD BE MOVED TO MACHINE DEPENDENT CODE */
	union {
		float r;
		expr_t e;
	} gross;
 /* end machine dependent */
	char hackbuf[50];
	double atof();

	for (i = sizeof(hackbuf), p = hackbuf; isdigit(readchar());)
		if (--i > 0)
			*p++ = lastc;
	*p = 0;
	gross.r = expv + atof(hackbuf);
	expv = gross.e;
}

/*
 * item : number | name [ '.' local ] | '.' local | '.' | '+' | '^' | '"' |
 *	  '<' var | '<' register | '\'' char(s) '\'' ;
 *
 * item returns 1 if it finds an item, or 0 if it resolves to
 * the empty string.
 */
static int
item(allownil)
	int allownil;
{
	register int i, c;
	struct reglist *reg;

	c = readchar();
	if (isdigit(c)) {
		getnum();
		return (1);
	}
	if (symchar(0)) {
		ev_name();
		return (1);
	}
	switch (c) {

	case '.':
		if (symchar(SYMCH_READ))
			ev_local();	/* SHOULD RESET xxxsym FIRST? */
		else
			expv = dot;
		unreadc();
		return (1);

	case '"':
		expv = ditto;
		return (1);

	case '+':
		expv = inkdot(dotinc);
		return (1);

	case '^':
		expv = inkdot(-dotinc);
		return (1);

	case '<':
		if ((reg = reglookup()) != NULL) {
			expv = getreg(reg);
			return (1);
		}
		else if ((i = varlookup(rdc())) != -1)
			expv = var[i];
		else
			error(BADVAR);
		return (1);

	case '\'':
		i = sizeof(expr_t) / sizeof(char);
		for (expv = 0;; expv = (expv << NBBY) | c) {
			if ((c = readchar()) == '\\') {
				if ((c = readchar()) == 0)
					break;
			} else if (c == '\'')
				break;
			if (--i < 0)
				error(BADSYN);
		}
		return (1);
	}
	if (!allownil)
		error(NOADR);
	unreadc();
	return (0);
}

/*
 * term : item | monadic_op term | '(' expr ')' ;
 */
term(allownil)
	int allownil;
{

	switch (readchar()) {

	case '*':
	case '@':
		(void) term(0);
		(void) adbread(lastc == '@' ? SP_INSTR : SP_DATA,
		    (addr_t)expv, (caddr_t)&expv, sizeof(expv));
		checkerr();
		return (1);

	case '-':
		(void) term(0);
		expv = -expv;
		return (1);

	case '~':
		(void) term(0);
		expv = ~expv;
		return (1);

	case '#':
		(void) term(0);
		expv = !expv;
		return (1);

	case '(':
		(void) iexpr(0);
		if (readchar() != ')')
			error(BADSYN);
		return (1);

	default:
		unreadc();
		return (item(allownil));
	}
}

/*
 * expr : term | term dyadic expr | ;
 * (internal version, which passes on the allow-nil flag)
 */
static int
iexpr(allownil)
	int allownil;
{
	register expr_t lhs, t;

	(void) rdc();
	unreadc();
	if (!term(allownil))
		return (0);
	for (;;) {
		lhs = expv;
		switch (readchar()) {

		case '+':
			(void) term(0);
			expv += lhs;
			break;

		case '-':
			(void) term(0);
			expv = lhs - expv;
			break;

		case '#':
			(void) term(0);
			if (expv == 0)
				error("# by 0");
			/* roundup(lhs, expv), but careful about overflow */
			t = lhs / expv;
			t *= expv;
			expv = t == lhs ? t : t + expv;
			break;

		case '*':
			(void) term(0);
			expv *= lhs;
			break;

		case '%':
			(void) term(0);
			expv = lhs / expv;
			break;

		case '&':
			(void) term(0);
			expv &= lhs;
			break;

		case '|':
			(void) term(0);
			expv |= lhs;
			break;

		default:
			unreadc();
			return (1);
		}
	}
}

int
oexpr()
{

	return (iexpr(1));
}

expr_t
rexpr()
{

	(void) iexpr(0);
	return (expv);
}

/*
 * Evaluate a name, or a name '.' localname.
 */
static
ev_name()
{
	struct nlist *symp;
	char symbuf[SYMLEN];

	/* name [ . localname ] */
	getsym(symbuf, sizeof(symbuf));
	if (lastc == '.')	/* name . local */
		find_frame(symbuf);
	else if ((symp = lookup(symbuf)) != NULL)
		expv = (xxxsym = symp)->n_value;
	else
		error(BADSYM);
	unreadc();
}

/*
 * Backtrack through the call stack to find the symbol in symbuf.
 * Save the result, and if there is another name, look for it within
 * that frame.  Otherwise the value of the expression is the address
 * of the found frame.
 */
static
find_frame(symbuf)
	char *symbuf;
{
	struct activation a;
	addr_t dummy;		/* for findsym() to scribble on */

	if (pid == 0)
		error(NOPCS);
	for (a_init(&a); a.a_valid; a_back(&a)) {
		checkerr();
		if ((xxxsym = findsym(a.a_pc, SP_INSTR, &dummy)) == NULL)
			break;
		if (eqsym(xxxsym->n_un.n_name, symbuf, '_')) {
			curframe = a;
			if (symchar(SYMCH_READ))
				ev_local();
			else
				expv = a.a_fp;
			return;
		}
	}
	error(NOCFN);
	/* NOTREACHED */
}

/*
 * Linear search (ugh) for a symbol in the current stack frame.
 */
static
ev_local()
{
	register struct nlist *sp;
	register char *a, *b;
	char symbuf[SYMLEN];

	if (pid == 0)
		error(NOPCS);
	if (!curframe.a_valid || (sp = xxxsym) == NULL)
		error(NOCFN);
	getsym(symbuf, SYMLEN);
	while ((sp = nextlocal(sp)) != NULL) {
		/*
		 * Local and parameter symbols (as generated by .stabs)
		 * end with ':', not '\0'; here we allow both.
		 */
		if (*(a = sp->n_un.n_name) != *(b = symbuf))
			continue;
		while (*a == *b++)
			if (*a++ == 0 || *a == ':') {
				expv = eval_localsym(sp, &curframe);
				xxxsym = sp;	/* ??? */
				return;
			}
	}
	error(BADLOC);
}

#ifndef inkdot
/*
 * Function version of inkdot().  Compute the new dot, and check for
 * address wrap-around.
 */
addr_t
inkdot(incr)
	int incr;
{
	addr_t newdot = dot + incr;

	if (ADDRESS_WRAP(dot, newdot))
		error(ADWRAP);
	return (newdot);
}
#endif
