/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "tree.h"
#include "opcode.h"

/*
 * The structure used to
 * hold information about
 * each case label.
 */
struct ct {
	long	clong;
	int	cline;
};

/*
 * Caseop generates the
 * pascal case statement code
 */
caseop(r)
	int *r;
{
	register struct nl *p;
	register struct ct *ctab;
	register *cs;
	int *cl;
	double low, high;
	int *brtab;
	char *brtab0;
	char *csend;
	int w, i, j, m, n;
	int nr, goc;

	goc = gocnt;
	/*
	 * Obtain selector attributes:
	 *	p	type
	 *	w	width
	 *	low	lwb(p)
	 *	high	upb(p)
	 */
	p = rvalue(r[2], NIL);
	if (p != NIL) {
		if (isnta(p, "bcsi")) {
			error("Case selectors cannot be %ss", nameof(p));
			p = NIL;
		} else {
			cl = p;
			if (p->class != RANGE)
				cl = p->type;
			if (cl == NIL)
				p = NIL;
			else {
				w = width(p);
#ifdef DEBUG
				if (hp21mx)
					w = 2;
#endif
				low = cl->range[0];
				high = cl->range[1];
			}
		}
	}
	/*
	 * Count # of cases
	 */
	n = 0;
	for (cl = r[3]; cl != NIL; cl = cl[2]) {
		cs = cl[1];
		if (cs == NIL)
			continue;
		for (cs = cs[2]; cs != NIL; cs = cs[2])
			n++;
	}
	/*
	 * Allocate case table space
	 */
	ctab = i = alloc(n * sizeof *ctab);
	if (i == -1) {
		error("Ran out of memory (case)");
		pexit(DIED);
	}
	/*
	 * Check the legality of the
	 * labels and count the number
	 * of good labels
	 */
	m = 0;
	for (cl = r[3]; cl != NIL; cl = cl[2]) {
		cs = cl[1];
		if (cs == NIL)
			continue;
		line = cs[1];
		for (cs = cs[2]; cs != NIL; cs = cs[2]) {
			gconst(cs[1]);
			if (p == NIL || con.ctype == NIL)
				continue;
			if (incompat(con.ctype, p, NIL)) {
				cerror("Case label type clashed with case selector expression type");
				continue;
			}
			if (con.crval < low || con.crval > high) {
				error("Case label out of range");
				continue;
			}
			ctab[m].clong = con.crval;
			ctab[m].cline = line;
			m++;
		}
	}

	/*
	 * Check for duplicate labels
	 */
	for (i = 0; i < m; i++)
		for (j = 0; j < m; j++)
			if (ctab[i].clong == ctab[j].clong) {
				if (i == j)
					continue;
				if (j < i)
					break;
				error("Multiply defined label in case, lines %d and %d", ctab[i].cline, ctab[j].cline);
			}
	/*
	 * Put out case operator and
	 * leave space for the
	 * branch table
	 */
	if (p != NIL) {
		put2(O_CASE1OP + (w >> 1), n);
		brtab = brtab0 = lc;
		putspace(n * 2);
		put1(O_CASEBEG);
		for (i=0; i<m; i++)
			put3(O_CASE1 + (w >> 1), ctab[i].clong);
		put1(O_CASEEND);
	}
	csend = getlab();
	put2(O_TRA, csend);
	/*
	 * Free the case
	 * table space.
	 */
	free(ctab);
	/*
	 * Generate code for each
	 * statement. Patch branch
	 * table to beginning of each
	 * statement and follow each
	 * statement with a branch back
	 * to the TRA above.
	 */
	nr = 1;
	for (cl = r[3]; cl != NIL; cl = cl[2]) {
		cs = cl[1];
		if (cs == NIL)
			continue;
		if (p != NIL)
			for (cs = cs[2]; cs != NIL; cs = cs[2]) {
				patchfil(brtab - 1, lc - brtab0);
				brtab++;
			}
		cs = cl[1];
		putcnt();
		level++;
		statement(cs[3]);
		nr =& noreach;
		noreach = 0;
		put2(O_TRA, csend);
		level--;
		if (gotos[cbn])
			ungoto();
	}
	/*
	 * Patch the termination branch
	 */
	patch(csend);
	noreach = nr;
	if (goc != gocnt)
		putcnt();
}
