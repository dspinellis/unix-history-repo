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
static char sccsid[] = "@(#)compkl.c	5.3 (Berkeley) 6/18/88";
#endif /* not lint */

# include	"trek.h"

/*
**  compute klingon distances
**
**	The klingon list has the distances for all klingons recomputed
**	and sorted.  The parameter is a Boolean flag which is set if
**	we have just entered a new quadrant.
**
**	This routine is used every time the Enterprise or the Klingons
**	move.
*/

compkldist(f)
int	f;		/* set if new quadrant */
{
	register int		i, dx, dy;
	double			d;
	double			temp;

	if (Etc.nkling == 0)
		return;
	for (i = 0; i < Etc.nkling; i++)
	{
		/* compute distance to the Klingon */
		dx = Ship.sectx - Etc.klingon[i].x;
		dy = Ship.secty - Etc.klingon[i].y;
		d = dx * dx + dy * dy;
		d = sqrt(d);

		/* compute average of new and old distances to Klingon */
		if (!f)
		{
			temp = Etc.klingon[i].dist;
			Etc.klingon[i].avgdist = 0.5 * (temp + d);
		}
		else
		{
			/* new quadrant: average is current */
			Etc.klingon[i].avgdist = d;
		}
		Etc.klingon[i].dist = d;
	}

	/* leave them sorted */
	sortkl();
}


/*
**  sort klingons
**
**	bubble sort on ascending distance
*/

sortkl()
{
	struct kling		t;
	register int		f, i, m;

	m = Etc.nkling - 1;
	f = 1;
	while (f)
	{
		f = 0;
		for (i = 0; i < m; i++)
			if (Etc.klingon[i].dist > Etc.klingon[i+1].dist)
			{
				bmove(&Etc.klingon[i], &t, sizeof t);
				bmove(&Etc.klingon[i+1], &Etc.klingon[i], sizeof t);
				bmove(&t, &Etc.klingon[i+1], sizeof t);
				f = 1;
			}
	}
}
