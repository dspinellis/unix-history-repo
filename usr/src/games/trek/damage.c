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
static char sccsid[] = "@(#)damage.c	5.2 (Berkeley) %G%";
#endif /* not lint */

# include	"trek.h"

/*
**  Schedule Ship.damages to a Device
**
**	Device `dev1' is damaged in an amount `dam'.  Dam is measured
**	in stardates, and is an additional amount of damage.  It should
**	be the amount to occur in non-docked mode.  The adjustment
**	to docked mode occurs automatically if we are docked.
**
**	Note that the repair of the device occurs on a DATE, meaning
**	that the dock() and undock() have to reschedule the event.
*/

damage(dev1, dam)
int	dev1;		/*  device index */
double	dam;		/* time to repair */
{
	register int		i;
	register struct event	*e;
	int			f;
	register int		dev;

	/* ignore zero damages */
	if (dam <= 0.0)
		return;
	dev = dev1;

	printf("\t%s damaged\n", Device[dev].name);

	/* find actual length till it will be fixed */
	if (Ship.cond == DOCKED)
		dam *= Param.dockfac;
	/* set the damage flag */
	f = damaged(dev);
	if (!f)
	{
		/* new damages -- schedule a fix */
		schedule(E_FIXDV, dam, 0, 0, dev);
		return;
	}
	/* device already damaged -- add to existing damages */
	/* scan for old damages */
	for (i = 0; i < MAXEVENTS; i++)
	{
		e = &Event[i];
		if (e->evcode != E_FIXDV || e->systemname != dev)
			continue;
		/* got the right one; add on the new damages */
		reschedule(e, e->date - Now.date + dam);
		return;
	}
	syserr("Cannot find old damages %d\n", dev);
}
