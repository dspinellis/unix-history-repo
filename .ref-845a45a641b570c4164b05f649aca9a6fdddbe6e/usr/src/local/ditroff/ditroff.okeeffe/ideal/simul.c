#ifndef lint
static char *sccsid ="simul.c	(CWI)	1.1	85/03/01";
#endif
#include "ideal.h"


DEPPTR depadd(dlistone, coeffone, dlisttwo, coefftwo)
DEPPTR dlistone;
float coeffone;
DEPPTR dlisttwo;
float coefftwo;
{
	/* produce a dependency list = coeffone*dlistone + coefftwo*dlisttwo */
	register DEPPTR onewalk, twowalk, newhead, newwalk, prevnew;
	DEPNODE nuhead;
	prevnew = &nuhead;
	prevnew->next = NULL;
	onewalk = dlistone;
	twowalk = dlisttwo;
	while ((onewalk != NULL) || (twowalk != NULL)) {
		if (onewalk != NULL)
			if (twowalk != NULL)
				if (onewalk->var > twowalk ->var) {
					newwalk = depgen (
						onewalk->var,
						coeffone*onewalk->coeff
					);
					onewalk = onewalk->next;
				}
				else
					if (onewalk->var == twowalk->var) {
						newwalk = depgen (
							onewalk->var,
							coeffone*onewalk->coeff
							+ coefftwo*twowalk->coeff
						);
						onewalk = onewalk->next;
						twowalk = twowalk->next;
					}
					else {
						newwalk = depgen (
							twowalk->var,
							coefftwo*twowalk->coeff
						);
						twowalk = twowalk->next;
					}
			else {
				newwalk = depgen (
					onewalk->var,
					coeffone*onewalk->coeff
				);
				onewalk = onewalk->next;
			}
		else {
			newwalk = depgen (
				twowalk->var,
				coefftwo*twowalk->coeff
			);
			twowalk = twowalk->next;
		}
		if (fabs(newwalk->coeff) > EPSILON) {
			prevnew->next = newwalk;
			prevnew = newwalk;
		} else
			depfree (newwalk);
	}
	newhead = nuhead.next;
	if (newhead != NULL) {
		if (dbg) {
			depprint (newhead);
			fprintf (stderr, "\n");
		}
		return(newhead);
	}
	else {
		dprintf "empty dep rep\n");
		return (depgen ((VARPTR) NULL, 0.0));
	}
}

DEPPTR depsubst(depinto, depfrom, depwho)
	/* substitutes depfrom for depwho in depinto
	/* WARNING:  if depinto actually contains depfrom,
	/* depinto is replaced */
DEPPTR depinto,
	depfrom;
VARPTR depwho;
{
	DEPPTR intowalker, intoparent, temp;
	for (intowalker = depinto;
	    intowalker != NULL;
	    intowalker = intowalker->next)
		if (intowalker->var == depwho)
			break;
		else
			intoparent = intowalker;
	if (intowalker == NULL)
		return(depinto);
	if (intowalker == depinto)
		depinto = depinto->next;
	else
		intoparent->next = intowalker->next;
	dprintf "Variable substitution proceeding\n");
	temp = depadd(depinto, 1.0, depfrom, intowalker->coeff);
	depfree (depinto);
	tryfree(intowalker);
	depinto = temp;
	return (temp);
}
