/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: term.c,v 2.5 85/08/22 16:09:34 timo Exp $ */

/*
 * B editor -- Init/end terminal-related modules.
 *
 * This file should be wiped out completely.
 */


#include "b.h"
#include "erro.h"

extern bool dflag;

extern bool nosense;


/*
 * Call initialization code of other terminal-dependent modules.
 * N.B. the order of initializations is determined by black magic.
 *	Don't change!
 */

Visible Procedure
initterm()
{
#ifndef NDEBUG
	if (dflag)
		fprintf(stderr, "*** initterm();\n\r");
#endif NDEBUG
	/* initshow(); */
	initgetc();
}


/*
 * Extermination code, reverse of initterm().
 * N.B. the order of exterminations is determined by black magic.
 *      Don't change!
 */

Visible Procedure
endterm()
{
#ifndef NDEBUG
	if (dflag)
		fprintf(stderr, "*** endterm();\n\r");
#endif NDEBUG
	/* endshow(); */
	endgetc();
}


/*
 * Compatible interface with trmsense; return No if not sensed.
 */

Visible bool
sense(py, px)
	int *py;
	int *px;
{
	trmsense(py, px);
	if (*py >= 0 && *px >= 0)
		return Yes;
	if (nosense)
		error(GOTO_NO);
	else
		error(GOTO_BAD);
	return No;
}
