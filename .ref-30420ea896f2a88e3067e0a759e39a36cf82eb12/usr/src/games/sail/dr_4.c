#ifndef lint
static	char *sccsid = "@(#)dr_4.c	1.2 83/07/20";
#endif
#include "externs.h"

ungrap(from, to)
register struct ship *from, *to;
{
	register k;
	register struct snag *sp = from->file->grapples;

	if (grappled2(from, to)) {
		for (k = 0; k < NSHIP; k++, sp++) {
			if (sp->turnfoul == 0 || to != sp->toship)
				continue;
			if (from->nationality == to->nationality && die() >= 3)
				continue;
			cleangrapple(from, to, k);
			makesignal(from, "ungrappling %s (%c%c)", to);
		}
	}
}

grap(from, to)
register struct ship *from, *to;
{
	register l;

	if (from->nationality != capship(to)->nationality && die() >= 3)
		return;
	for (l = 0; l < NSHIP && from->file->grapples[l].turnfoul; l++)
		;
	if (l < NSHIP)
		Write(W_GRAP, from, 0, l, turn, to-SHIP(0), 0);
	for (l = 0; l < NSHIP && to->file->grapples[l].turnfoul; l++)
		;
	if (l < NSHIP)
		Write(W_GRAP, to, 0, l, turn, from-SHIP(0), 0);
	makesignal(from, "grappled with %s (%c%c)", to);
}
