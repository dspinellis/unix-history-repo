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
static char sccsid[] = "@(#)capture.c	5.3 (Berkeley) %G%";
#endif /* not lint */

# include	"trek.h"

/*
**  Ask a Klingon To Surrender
**
**	(Fat chance)
**
**	The Subspace Radio is needed to ask a Klingon if he will kindly
**	surrender.  A random Klingon from the ones in the quadrant is
**	chosen.
**
**	The Klingon is requested to surrender.  The probability of this
**	is a function of that Klingon's remaining power, our power,
**	etc.
*/

capture()
{
	register int		i;
	register struct kling	*k;
	double			x;
	extern struct kling	*selectklingon();

	/* check for not cloaked */
	if (Ship.cloaked)
	{
		printf("Ship-ship communications out when cloaked\n");
		return;
	}
	if (damaged(SSRADIO))
		return (out(SSRADIO));
	/* find out if there are any at all */
	if (Etc.nkling <= 0)
	{
		printf("Uhura: Getting no response, sir\n");
		return;
	}

	/* if there is more than one Klingon, find out which one */
	k = selectklingon();
	Move.free = 0;
	Move.time = 0.05;

	/* check out that Klingon */
	k->srndreq++;
	x = Param.klingpwr;
	x *= Ship.energy;
	x /= k->power * Etc.nkling;
	x *= Param.srndrprob;
	i = x;
#	ifdef xTRACE
	if (Trace)
		printf("Prob = %d (%.4f)\n", i, x);
#	endif
	if (i > ranf(100))
	{
		/* guess what, he surrendered!!! */
		printf("Klingon at %d,%d surrenders\n", k->x, k->y);
		i = ranf(Param.klingcrew);
		if ( i > 0 )
			printf("%d klingons commit suicide rather than be taken captive\n", Param.klingcrew - i);
		if (i > Ship.brigfree)
			i = Ship.brigfree;
		Ship.brigfree -= i;
		printf("%d captives taken\n", i);
		killk(k->x, k->y);
		return;
	}

	/* big surprise, he refuses to surrender */
	printf("Fat chance, captain\n");
	return;
}


/*
**  SELECT A KLINGON
**
**	Cruddy, just takes one at random.  Should ask the captain.
*/

struct kling	*selectklingon()
{
	register int		i;

	if (Etc.nkling < 2)
		i = 0;
	else
		i = ranf(Etc.nkling);
	return (&Etc.klingon[i]);
}
