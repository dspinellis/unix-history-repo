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
static char sccsid[] = "@(#)abandon.c	5.3 (Berkeley) %G%";
#endif /* not lint */

# include	"trek.h"

/*
**  Abandon Ship
**
**	The ship is abandoned.  If your current ship is the Faire
**	Queene, or if your shuttlecraft is dead, you're out of
**	luck.  You need the shuttlecraft in order for the captain
**	(that's you!!) to escape.
**
**	Your crew can beam to an inhabited starsystem in the
**	quadrant, if there is one and if the transporter is working.
**	If there is no inhabited starsystem, or if the transporter
**	is out, they are left to die in outer space.
**
**	These currently just count as regular deaths, but they
**	should count very heavily against you.
**
**	If there are no starbases left, you are captured by the
**	Klingons, who torture you mercilessly.  However, if there
**	is at least one starbase, you are returned to the
**	Federation in a prisoner of war exchange.  Of course, this
**	can't happen unless you have taken some prisoners.
**
**	Uses trace flag 40
*/

abandon()
{
	register struct quad	*q;
	register int		i;
	int			j;
	register struct event	*e;

	if (Ship.ship == QUEENE)
		return (printf("You may not abandon ye Faire Queene\n"));
	if (Ship.cond != DOCKED)
	{
		if (damaged(SHUTTLE))
			return (out(SHUTTLE));
		printf("Officers escape in shuttlecraft\n");
		/* decide on fate of crew */
		q = &Quad[Ship.quadx][Ship.quady];
		if (q->qsystemname == 0 || damaged(XPORTER))
		{
			printf("Entire crew of %d left to die in outer space\n",
				Ship.crew);
			Game.deaths += Ship.crew;
		}
		else
		{
			printf("Crew beams down to planet %s\n", systemname(q));
		}
	}
	/* see if you can be exchanged */
	if (Now.bases == 0 || Game.captives < 20 * Game.skill)
		lose(L_CAPTURED);
	/* re-outfit new ship */
	printf("You are hereby put in charge of an antiquated but still\n");
	printf("  functional ship, the Fairie Queene.\n");
	Ship.ship = QUEENE;
	Ship.shipname = "Fairie Queene";
	Param.energy = Ship.energy = 3000;
	Param.torped = Ship.torped = 6;
	Param.shield = Ship.shield = 1250;
	Ship.shldup = 0;
	Ship.cloaked = 0;
	Ship.warp = 5.0;
	Ship.warp2 = 25.0;
	Ship.warp3 = 125.0;
	Ship.cond = GREEN;
	/* clear out damages on old ship */
	for (i = 0; i < MAXEVENTS; i++)
	{
		e = &Event[i];
		if (e->evcode != E_FIXDV)
			continue;
		unschedule(e);
	}
	/* get rid of some devices and redistribute probabilities */
	i = Param.damprob[SHUTTLE] + Param.damprob[CLOAK];
	Param.damprob[SHUTTLE] = Param.damprob[CLOAK] = 0;
	while (i > 0)
		for (j = 0; j < NDEV; j++)
		{
			if (Param.damprob[j] != 0)
			{
				Param.damprob[j] += 1;
				i--;
				if (i <= 0)
					break;
			}
		}
	/* pick a starbase to restart at */
	i = ranf(Now.bases);
	Ship.quadx = Now.base[i].x;
	Ship.quady = Now.base[i].y;
	/* setup that quadrant */
	while (1)
	{
		initquad(1);
		Sect[Ship.sectx][Ship.secty] = EMPTY;
		for (i = 0; i < 5; i++)
		{
			Ship.sectx = Etc.starbase.x + ranf(3) - 1;
			if (Ship.sectx < 0 || Ship.sectx >= NSECTS)
				continue;
			Ship.secty = Etc.starbase.y + ranf(3) - 1;
			if (Ship.secty < 0 || Ship.secty >= NSECTS)
				continue;
			if (Sect[Ship.sectx][Ship.secty] == EMPTY)
			{
				Sect[Ship.sectx][Ship.secty] = QUEENE;
				dock();
				compkldist(0);
				return;
			}
		}
	}
}
