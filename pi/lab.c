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
#include "opcode.h"

/*
 * Label enters the definitions
 * of the label declaration part
 * into the namelist.
 */
label(r)
	int *r;
{
	register *ll;
	register struct nl *p, *lp;
	int l;

	lp = NIL;
	if (parts & (CPRT|TPRT|VPRT))
		error("Label declarations must precede const, type and var declarations");
	if (parts & LPRT)
		error("All labels must be declared in one label part");
	parts =| LPRT;
	for (ll = r; ll != NIL; ll = ll[2]) {
		l = getlab();
		p = enter(defnl(ll[1], LABEL, 0, l));
		/*
		 * Get the label for the eventual target
		 */
		p->value[1] = getlab();
		p->chain = lp;
		p->nl_flags =| (NFORWD|NMOD);
		p->value[NL_GOLEV] = NOTYET;
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
}

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
	put2(O_TRA, p->value[0]);
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
	p->nl_flags =& ~NFORWD;
	patch(p->value[1]);
	if (p->value[NL_GOLEV] != NOTYET)
		if (p->value[NL_GOLEV] < level) {
			recovered();
			error("Goto %s from line %d is into a structured statement", s, p->value[NL_GOLINE]);
		}
	p->value[NL_GOLEV] = level;
}
