/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 November 1978
 */

#include "whoami"
#include "0.h"
#include "tree.h"
#include "opcode.h"

/*
 * Label enters the definitions
 * of the label declaration part
 * into the namelist.
 */
label(r, l)
	int *r, l;
{
#ifndef PI0
	register *ll;
	register struct nl *p, *lp;

	lp = NIL;
#else
	send(REVLAB, r);
#endif
	line = l;
#ifndef PI1
	if (parts & (CPRT|TPRT|VPRT))
		error("Label declarations must precede const, type and var declarations");
	if (parts & LPRT)
		error("All labels must be declared in one label part");
	parts |= LPRT;
#endif
#ifndef PI0
	for (ll = r; ll != NIL; ll = ll[2]) {
		l = getlab();
		p = enter(defnl(ll[1], LABEL, 0, l));
		/*
		 * Get the label for the eventual target
		 */
		p->value[1] = getlab();
		p->chain = lp;
		p->nl_flags |= (NFORWD|NMOD);
		p->value[NL_GOLEV] = NOTYET;
		p->entloc = l;
		lp = p;
		/*
		 * This operator is between
		 * the bodies of two procedures
		 * and provides a target for
		 * gotos for this label via TRA.
		 */
		putlab(l);
		put2(O_GOTO | cbn<<9, p->value[1]);
	}
	gotos[cbn] = lp;
#	ifdef PTREE
	    {
		pPointer	Labels = LabelDCopy( r );

		pDEF( PorFHeader[ nesting ] ).PorFLabels = Labels;
	    }
#	endif
#endif
}

#ifndef PI0
/*
 * Gotoop is called when
 * we get a statement "goto label"
 * and generates the needed tra.
 */
gotoop(s)
	char *s;
{
	register struct nl *p;

	gocnt++;
	p = lookup(s);
	if (p == NIL)
		return (NIL);
	put2(O_TRA4, p->entloc);
	if (bn == cbn)
		if (p->nl_flags & NFORWD) {
			if (p->value[NL_GOLEV] == NOTYET) {
				p->value[NL_GOLEV] = level;
				p->value[NL_GOLINE] = line;
			}
		} else
			if (p->value[NL_GOLEV] == DEAD) {
				recovered();
				error("Goto %s is into a structured statement", p->symbol);
			}
}

/*
 * Labeled is called when a label
 * definition is encountered, and
 * marks that it has been found and
 * patches the associated GOTO generated
 * by gotoop.
 */
labeled(s)
	char *s;
{
	register struct nl *p;

	p = lookup(s);
	if (p == NIL)
		return (NIL);
	if (bn != cbn) {
		error("Label %s not defined in correct block", s);
		return;
	}
	if ((p->nl_flags & NFORWD) == 0) {
		error("Label %s redefined", s);
		return;
	}
	p->nl_flags &= ~NFORWD;
	patch4(p->entloc);
	if (p->value[NL_GOLEV] != NOTYET)
		if (p->value[NL_GOLEV] < level) {
			recovered();
			error("Goto %s from line %d is into a structured statement", s, p->value[NL_GOLINE]);
		}
	p->value[NL_GOLEV] = level;
}
#endif
