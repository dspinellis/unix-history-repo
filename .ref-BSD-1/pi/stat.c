#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
#include "0.h"
#include "tree.h"

int cntstat;
int cnts 2;
#include "opcode.h"

/*
 * Statement list
 */
statlist(r)
	int *r;
{
	register *sl;

	for (sl=r; sl != NIL; sl=sl[2])
		statement(sl[1]);
}

/*
 * Statement
 */
statement(r)
	int *r;
{
	register *s;
	register struct nl *snlp;
/*
	register sudcnt;
*/

	s = r;
	snlp = nlp;
/*
	sudcnt = udcnt;
*/
top:
	if (cntstat) {
		cntstat = 0;
		putcnt();
	}
	if (s == NIL)
		return;
	line = s[1];
	if (s[0] == T_LABEL) {
		labeled(s[2]);
		s = s[3];
		noreach = 0;
		cntstat = 1;
		goto top;
	}
	if (noreach) {
		noreach = 0;
		warning();
		error("Unreachable statement");
	}
	switch (s[0]) {
		case T_PCALL:
			putline();
			proc(s);
			break;
		case T_ASGN:
			putline();
			asgnop(s);
			break;
		case T_GOTO:
			putline();
			gotoop(s[2]);
			noreach = 1;
			cntstat = 1;
			break;
		default:
			level++;
			switch (s[0]) {
				default:
					panic("stat");
				case T_IF:
				case T_IFEL:
					ifop(s);
					break;
				case T_WHILE:
					whilop(s);
					noreach = 0;
					break;
				case T_REPEAT:
					repop(s);
					break;
				case T_FORU:
				case T_FORD:
					forop(s);
					noreach = 0;
					break;
				case T_BLOCK:
					statlist(s[2]);
					break;
				case T_CASE:
					putline();
					caseop(s);
					break;
				case T_WITH:
					withop(s);
					break;
				case T_ASRT:
					putline();
					asrtop(s);
					break;
			}
			--level;
			if (gotos[cbn])
				ungoto();
			break;
	}
	/*
	 * Free the temporary name list entries defined in
	 * expressions, e.g. STRs, and WITHPTRs from withs.
	 */
/*
	if (sudcnt == udcnt)
*/
		nlfree(snlp);
}

ungoto()
{
	register struct nl *p;

	for (p = gotos[cbn]; p != NIL; p = p->chain)
		if ((p->nl_flags & NFORWD) != 0) {
			if (p->value[NL_GOLEV] != NOTYET)
				if (p->value[NL_GOLEV] > level)
					p->value[NL_GOLEV] = level;
		} else
			if (p->value[NL_GOLEV] != DEAD)
				if (p->value[NL_GOLEV] > level)
					p->value[NL_GOLEV] = DEAD;
}

putcnt()
{

	if (monflg == 0)
		return;
	cnts++;
	put2(O_COUNT, cnts);
}

putline()
{

	if (opt('p') != 0)
		put2(O_LINO, line);
}

/*
 * With varlist do stat
 *
 * With statement requires an extra word
 * in automatic storage for each level of withing.
 * These indirect pointers are initialized here, and
 * the scoping effect of the with statement occurs
 * because lookup examines the field names of the records
 * associated with the WITHPTRs on the withlist.
 */
withop(s)
	int *s;
{
	register *p;
	register struct nl *r;
	int i;
	int *swl;
	long soffset;

	putline();
	swl = withlist;
	soffset = sizes[cbn].om_off;
	for (p = s[2]; p != NIL; p = p[2]) {
		sizes[cbn].om_off =- 2;
		put2(O_LV | cbn <<9, i = sizes[cbn].om_off);
		r = lvalue(p[1], MOD);
		if (r == NIL)
			continue;
		if (r->class != RECORD) {
			error("Variable in with statement refers to %s, not to a record", nameof(r));
			continue;
		}
		r = defnl(0, WITHPTR, r, i);
		r->nl_next = withlist;
		withlist = r;
		put1(O_AS2);
	}
	if (sizes[cbn].om_off < sizes[cbn].om_max)
		sizes[cbn].om_max = sizes[cbn].om_off;
	statement(s[3]);
	sizes[cbn].om_off = soffset;
	withlist = swl;
}

extern	flagwas;
/*
 * var := expr
 */
asgnop(r)
	int *r;
{
	register struct nl *p;
	register *av;

	if (r == NIL)
		return (NIL);
	/*
	 * Asgnop's only function is
	 * to handle function variable
	 * assignments.  All other assignment
	 * stuff is handled by asgnop1.
	 */
	av = r[2];
	if (av != NIL && av[0] == T_VAR && av[3] == NIL) {
		p = lookup1(av[2]);
		if (p != NIL)
			p->nl_flags = flagwas;
		if (p != NIL && p->class == FVAR) {
			/*
			 * Give asgnop1 the func
			 * which is the chain of
			 * the FVAR.
			 */
			p->nl_flags =| NUSED|NMOD;
			p = p->chain;
			if (p == NIL) {
				rvalue(r[3], NIL);
				return;
			}
			put2(O_LV | bn << 9, p->value[NL_OFFS]);
			if (isa(p->type, "i") && width(p->type) == 1)
				asgnop1(r, nl+T2INT);
			else
				asgnop1(r, p->type);
			return;
		}
	}
	asgnop1(r, NIL);
}

/*
 * Asgnop1 handles all assignments.
 * If p is not nil then we are assigning
 * to a function variable, otherwise
 * we look the variable up ourselves.
 */
asgnop1(r, p)
	int *r;
	register struct nl *p;
{
	register struct nl *p1;

	if (r == NIL)
		return (NIL);
	if (p == NIL) {
		p = lvalue(r[2], MOD|ASGN|NOUSE);
		if (p == NIL) {
			rvalue(r[3], NIL);
			return (NIL);
		}
	}
	p1 = rvalue(r[3], p);
	if (p1 == NIL)
		return (NIL);
	if (incompat(p1, p, r[3])) {
		cerror("Type of expression clashed with type of variable in assignment");
		return (NIL);
	}
	switch (classify(p)) {
		case TBOOL:
		case TCHAR:
		case TINT:
		case TSCAL:
			rangechk(p, p1);
		case TDOUBLE:
		case TPTR:
			gen(O_AS2, O_AS2, width(p), width(p1));
			break;
		default:
			put2(O_AS, width(p));
	}
	return (p);	/* Used by for statement */
}

/*
 * for var := expr [down]to expr do stat
 */
forop(r)
	int *r;
{
	register struct nl *t1, *t2;
	int l1, l2, l3;
	long soffset;
	register op;
	struct nl *p;
	int *rr, goc, i;

	p = NIL;
	goc = gocnt;
	if (r == NIL)
		goto aloha;
	putline();
	/*
	 * Start with assignment
	 * of initial value to for variable
	 */
	t1 = asgnop1(r[2], NIL);
	if (t1 == NIL) {
		rvalue(r[3], NIL);
		statement(r[4]);
		goto aloha;
	}
	rr = r[2];		/* Assignment */
	rr = rr[2];		/* Lhs variable */
	if (rr[3] != NIL) {
		error("For variable must be unqualified");
		rvalue(r[3], NIL);
		statement(r[4]);
		goto aloha;
	}
	p = lookup(rr[2]);
	p->value[NL_FORV] = 1;
	if (isnta(t1, "bcis")) {
		error("For variables cannot be %ss", nameof(t1));
		statement(r[4]);
		goto aloha;
	}
	/*
	 * Allocate automatic
	 * space for limit variable
	 */
	sizes[cbn].om_off =- 4;
	if (sizes[cbn].om_off < sizes[cbn].om_max)
		sizes[cbn].om_max = sizes[cbn].om_off;
	i = sizes[cbn].om_off;
	/*
	 * Initialize the limit variable
	 */
	put2(O_LV | cbn<<9, i);
	t2 = rvalue(r[3], NIL);
	if (incompat(t2, t1, r[3])) {
		cerror("Limit type clashed with index type in 'for' statement");
		statement(r[4]);
		goto aloha;
	}
	put1(width(t2) <= 2 ? O_AS24 : O_AS4);
	/*
	 * See if we can skip the loop altogether
	 */
	rr = r[2];
	if (rr != NIL)
		rvalue(rr[2], NIL);
	put2(O_RV4 | cbn<<9, i);
	gen(NIL, r[0] == T_FORU ? T_LE : T_GE, width(t1), 4);
	/*
	 * L1 will be patched to skip the body of the loop.
	 * L2 marks the top of the loop when we go around.
	 */
	put2(O_IF, (l1 = getlab()));
	putlab(l2 = getlab());
	putcnt();
	statement(r[4]);
	/*
	 * now we see if we get to go again
	 */
	if (opt('t') == 0) {
		/*
		 * Easy if we dont have to test
		 */
		put2(O_RV4 | cbn<<9, i);
		if (rr != NIL)
			lvalue(rr[2], MOD);
		put2((r[0] == T_FORU ? O_FOR1U : O_FOR1D) + (width(t1) >> 1), l2);
	} else {
		line = r[1];
		putline();
		if (rr != NIL)
			rvalue(rr[2], NIL);
		put2(O_RV4 | cbn << 9, i);
		gen(NIL, (r[0] == T_FORU ? T_LT : T_GT), width(t1), 4);
		l3 = put2(O_IF, getlab());
		lvalue(rr[2], MOD);
		rvalue(rr[2], NIL);
		put2(O_CON2, 1);
		t2 = gen(NIL, r[0] == T_FORU ? T_ADD: T_SUB, width(t1), 2);
		rangechk(t1, t2);	/* The point of all this */
		gen(O_AS2, O_AS2, width(t1), width(t2));
		put2(O_TRA, l2);
		patch(l3);
	}
	sizes[cbn].om_off =+ 4;
	patch(l1);
aloha:
	noreach = 0;
	if (p != NIL)
		p->value[NL_FORV] = 0;
	if (goc != gocnt)
		putcnt();
}

/*
 * if expr then stat [ else stat ]
 */
ifop(r)
	int *r;
{
	register struct nl *p;
	register l1, l2;
	int nr, goc;

	goc = gocnt;
	if (r == NIL)
		return;
	putline();
	p = rvalue(r[2], NIL);
	if (p == NIL) {
		statement(r[3]);
		noreach = 0;
		statement(r[4]);
		noreach = 0;
		return;
	}
	if (isnta(p, "b")) {
		error("Type of expression in if statement must be Boolean, not %s", nameof(p));
		statement(r[3]);
		noreach = 0;
		statement(r[4]);
		noreach = 0;
		return;
	}
	l1 = put2(O_IF, getlab());
	putcnt();
	statement(r[3]);
	nr = noreach;
	if (r[4] != NIL) {
		/*
		 * else stat
		 */
		--level;
		ungoto();
		++level;
		l2 = put2(O_TRA, getlab());
		patch(l1);
		noreach = 0;
		statement(r[4]);
		noreach =& nr;
		l1 = l2;
	} else
		noreach = 0;
	patch(l1);
	if (goc != gocnt)
		putcnt();
}

/*
 * while expr do stat
 */
whilop(r)
	int *r;
{
	register struct nl *p;
	register l1, l2;
	int goc;

	goc = gocnt;
	if (r == NIL)
		return;
	putlab(l1 = getlab());
	putline();
	p = rvalue(r[2], NIL);
	if (p == NIL) {
		statement(r[3]);
		noreach = 0;
		return;
	}
	if (isnta(p, "b")) {
		error("Type of expression in while statement must be Boolean, not %s", nameof(p));
		statement(r[3]);
		noreach = 0;
		return;
	}
	put2(O_IF, (l2 = getlab()));
	putcnt();
	statement(r[3]);
	put2(O_TRA, l1);
	patch(l2);
	if (goc != gocnt)
		putcnt();
}

/*
 * repeat stat* until expr
 */
repop(r)
	int *r;
{
	register struct nl *p;
	register l;
	int goc;

	goc = gocnt;
	if (r == NIL)
		return;
	l = putlab(getlab());
	putcnt();
	statlist(r[2]);
	line = r[1];
	p = rvalue(r[3], NIL);
	if (p == NIL)
		return;
	if (isnta(p,"b")) {
		error("Until expression type must be Boolean, not %s, in repeat statement", nameof(p));
		return;
	}
	put2(O_IF, l);
	if (goc != gocnt)
		putcnt();
}

/*
 * assert expr
 */
asrtop(r)
	register int *r;
{
	register struct nl *q;

	if (opt('s')) {
		standard();
		error("Assert statement is non-standard");
	}
	if (!opt('t'))
		return;
	r = r[2];
	q = rvalue(r, NIL);
	if (q == NIL)
		return;
	if (isnta(q, "b"))
		error("Assert expression must be Boolean, not %ss", nameof(q));
	put1(O_ASRT);
}
