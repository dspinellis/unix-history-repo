/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)rest.c	5.2 (Berkeley) %G%";
#endif /* not lint */

# include	"trek.h"
# include	"getpar.h"

/*
**  REST FOR REPAIRS
**
**	You sit around and wait for repairs to happen.  Actually, you
**	sit around and wait for anything to happen.  I do want to point
**	out however, that Klingons are not as patient as you are, and
**	they tend to attack you while you are resting.
**
**	You can never rest through a long range tractor beam.
**
**	In events() you will be given an opportunity to cancel the
**	rest period if anything momentous happens.
*/

rest()
{
	double			t;
	register int		percent;

	/* get the time to rest */
	t = getfltpar("How long");
	if (t <= 0.0)
		return;
	percent = 100 * t / Now.time + 0.5;
	if (percent >= 70)
	{
		printf("Spock: That would take %d%% of our remaining time.\n",
			percent);
		if (!getynpar("Are you really certain that is wise"))
			return;
	}
	Move.time = t;

	/* boundary condition is the LRTB */
	t = Now.eventptr[E_LRTB]->date - Now.date;
	if (Ship.cond != DOCKED && Move.time > t)
		Move.time = t + 0.0001;
	Move.free = 0;
	Move.resting = 1;
}
