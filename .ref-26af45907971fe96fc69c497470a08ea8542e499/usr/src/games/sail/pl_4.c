/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)pl_4.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "player.h"

changesail()
{
	int rig, full;

	rig = mc->rig1;
	full = mf->FS;
	if (windspeed == 6 || windspeed == 5 && mc->class > 4)
		rig = 0;
	if (mc->crew3 && rig) {
		if (!full) {
			if (sgetch("Increase to Full sails? ",
				(struct ship *)0, 1) == 'y') {
				changed = 1;
				Write(W_FS, ms, 0, 1, 0, 0, 0);
			}
		} else {
			if (sgetch("Reduce to Battle sails? ",
				(struct ship *)0, 1) == 'y') {
				Write(W_FS, ms, 0, 0, 0, 0, 0);
				changed = 1;
			}
		}
	} else if (!rig)
		Signal("Sails rent to pieces", (struct ship *)0);
}

acceptsignal()
{
	char buf[60];
	register char *p = buf;

	*p++ = '"';
	sgetstr("Message? ", p, sizeof buf - 2);
	while (*p++)
		;
	p[-1] = '"';
	*p = 0;
	Write(W_SIGNAL, ms, 1, (int)buf, 0, 0, 0);
}

lookout()
{
	register struct ship *sp;
	char buf[3];
	register char c;

	sgetstr("What ship? ", buf, sizeof buf);
	foreachship(sp) {
		c = *countryname[sp->nationality];
		if ((c == *buf || tolower(c) == *buf || colours(sp) == *buf)
		    && (sp->file->stern == buf[1] || sterncolour(sp) == buf[1]
			|| buf[1] == '?')) {
			eyeball(sp);
		}
	}
}

char *
saywhat(sp, flag)
register struct ship *sp;
char flag;
{
	if (sp->file->captain[0])
		return sp->file->captain;
	else if (sp->file->struck)
		return "(struck)";
	else if (sp->file->captured != 0)
		return "(captured)";
	else if (flag)
		return "(available)";
	else
		return "(computer)";
}

eyeball(ship)
register struct ship *ship;
{
	int i;

	if (ship->file->dir != 0) {
		Signal("Sail ho! (range %d, %s)",
			(struct ship *)0, range(ms, ship), saywhat(ship, 0));
		i = portside(ms, ship, 1) - mf->dir;
		if (i <= 0)
			i += 8;
		Signal("%s (%c%c) %s %s %s.",
			ship, countryname[ship->nationality],
			classname[ship->specs->class], directionname[i]);
	}
}
