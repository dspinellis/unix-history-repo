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
static char sccsid[] = "@(#)check_out.c	5.2 (Berkeley) %G%";
#endif /* not lint */

# include	"trek.h"

/*
**  CHECK IF A DEVICE IS OUT
**
**	The indicated device is checked to see if it is disabled.  If
**	it is, an attempt is made to use the starbase device.  If both
**	of these fails, it returns non-zero (device is REALLY out),
**	otherwise it returns zero (I can get to it somehow).
**
**	It prints appropriate messages too.
*/

check_out(device)
int	device;
{
	register int	dev;

	dev = device;

	/* check for device ok */
	if (!damaged(dev))
		return (0);

	/* report it as being dead */
	out(dev);

	/* but if we are docked, we can go ahead anyhow */
	if (Ship.cond != DOCKED)
		return (1);
	printf("  Using starbase %s\n", Device[dev].name);
	return (0);
}
