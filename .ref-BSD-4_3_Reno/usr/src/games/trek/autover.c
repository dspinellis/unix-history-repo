/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)autover.c	5.5 (Berkeley) 6/1/90";
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

	printf("\07RED ALERT:  The %s is in a supernova quadrant\n", Ship.shipname);
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
