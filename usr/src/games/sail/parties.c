/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)parties.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include "externs.h"

meleeing(from, to)
struct ship *from;
register struct ship *to;
{
	register struct BP *p = from->file->OBP;
	register struct BP *q = p + NBP;

	for (; p < q; p++)
		if (p->turnsent && p->toship == to)
			return 1;
	return 0;
}

boarding(from, isdefense)
register struct ship *from;
char isdefense;
{
	register struct BP *p = isdefense ? from->file->DBP : from->file->OBP;
	register struct BP *q = p + NBP;

	for (; p < q; p++)
		if (p->turnsent)
			return 1;
	return 0;
}

unboard(ship, to, isdefense)
register struct ship *ship, *to;
register char isdefense;
{
	register struct BP *p = isdefense ? ship->file->DBP : ship->file->OBP;
	register n;

	for (n = 0; n < NBP; p++, n++)
		if (p->turnsent && (p->toship == to || isdefense || ship == to))
			Write(isdefense ? W_DBP : W_OBP, ship, 0, n, 0, 0, 0);
}
