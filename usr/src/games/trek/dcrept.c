/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)dcrept.c	5.3 (Berkeley) %G%";
#endif /* not lint */

# include	"trek.h"

/*
**  damage control report
**
**	Print damages and time to fix.  This is taken from the event
**	list.  A couple of factors are set up, based on whether or not
**	we are docked.  (One of these factors will always be 1.0.)
**	The event list is then scanned for damage fix events, the
**	time until they occur is determined, and printed out.  The
**	magic number DAMFAC is used to tell how much faster you can
**	fix things if you are docked.
*/

dcrept()
{
	register int		i, f;
	double			x;
	double			m1, m2;
	register struct event	*e;

	/* set up the magic factors to output the time till fixed */
	if (Ship.cond == DOCKED)
	{
		m1 = 1.0 / Param.dockfac;
		m2 = 1.0;
	}
	else
	{
		m1 = 1.0;
		m2 = Param.dockfac;
	}
	printf("Damage control report:\n");
	f = 1;

	/* scan for damages */
	for (i = 0; i < MAXEVENTS; i++)
	{
		e = &Event[i];
		if (e->evcode != E_FIXDV)
			continue;

		/* output the title first time */
		if (f)
		{
			printf("\t\t\t  repair times\n");
			printf("device\t\t\tin flight  docked\n");
			f = 0;
		}

		/* compute time till fixed, then adjust by the magic factors */
		x = e->date - Now.date;
		printf("%-24s%7.2f  %7.2f\n",
			Device[e->systemname].name, x * m1 + 0.005, x * m2 + 0.005);

		/* do a little consistancy checking */
	}

	/* if everything was ok, reassure the nervous captain */
	if (f)
		printf("All devices functional\n");
}
