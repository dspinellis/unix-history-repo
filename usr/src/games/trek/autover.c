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
static char sccsid[] = "@(#)autover.c	5.2 (Berkeley) %G%";
#endif /* not lint */

# include	"trek.h"

/*
**  Automatic Override
**
**	If we should be so unlucky as to be caught in a quadrant
**	with a supernova in it, this routine is called.  It is
**	called from checkcond().
**
**	It sets you to a random warp (guaranteed to be over 6.0)
**	and starts sending you off "somewhere" (whereever that is).
**
**	Please note that it is VERY important that you reset your
**	warp speed after the automatic override is called.  The new
**	warp factor does not stay in effect for just this routine.
**
**	This routine will never try to send you more than sqrt(2)
**	quadrants, since that is all that is needed.
*/

autover()
{
	double			dist;
	register int		course;

	printf("RED ALERT:  The %s is in a supernova quadrant\n", Ship.shipname);
	printf("***  Emergency override attempts to hurl %s to safety\n", Ship.shipname);
	/* let's get our ass out of here */
	Ship.warp = 6.0 + 2.0 * franf();
	Ship.warp2 = Ship.warp * Ship.warp;
	Ship.warp3 = Ship.warp2 * Ship.warp;
	dist = 0.75 * Ship.energy / (Ship.warp3 * (Ship.shldup + 1));
	if (dist > 1.4142)
		dist = 1.4142;
	course = ranf(360);
	Etc.nkling = -1;
	Ship.cond = RED;
	warp(-1, course, dist);
	attack(0);
}
