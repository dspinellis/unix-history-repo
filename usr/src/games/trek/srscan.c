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
static char sccsid[] = "@(#)srscan.c	5.3 (Berkeley) 6/18/88";
#endif /* not lint */

# include	"trek.h"
# include	"getpar.h"

/*
**  SHORT RANGE SENSOR SCAN
**
**	A short range scan is taken of the current quadrant.  If the
**	flag 'f' is one, it is an "auto srscan", which is not done
**	unless in 'fast' mode.  It does a status report and a srscan.
**	If 'f' is -1, you get a status report only.  If it is zero,
**	you get a srscan and an optional status report.  The status
**	report is taken if you enter "srscan yes"; for all srscans
**	thereafter you get a status report with your srscan until
**	you type "srscan no".  It defaults to on.
**
**	The current quadrant is filled in on the computer chart.
*/

char	*Color[4] =
{
	"GREEN",
	"DOCKED",
	"YELLOW",
	"RED"
};

srscan(f)
int	f;
{
	register int		i, j;
	register int		statinfo;
	char			*s;
	int			percent;
	struct quad		*q;
	extern struct cvntab	Skitab[];
	extern struct cvntab	Lentab[];
	struct cvntab		*p;

	if (f >= 0 && check_out(SRSCAN))
	{
		return;
	}
	if (f)
		statinfo = 1;
	else
	{
		if (!testnl())
			Etc.statreport = getynpar("status report");
		statinfo = Etc.statreport;
	}
	if (f > 0)
	{
		Etc.statreport = 1;
		if (!Etc.fast)
			return;
	}
	if (f >= 0)
	{
		printf("\nShort range sensor scan\n");
		q = &Quad[Ship.quadx][Ship.quady];
		q->scanned = q->klings * 100 + q->bases * 10 + q->stars;
		printf("  ");
		for (i = 0; i < NSECTS; i++)
		{
			printf("%d ", i);
		}
		printf("\n");
	}

	for (i = 0; i < NSECTS; i++)
	{
		if (f >= 0)
		{
			printf("%d ", i);
			for (j = 0; j < NSECTS; j++)
				printf("%c ", Sect[i][j]);
			printf("%d", i);
			if (statinfo)
				printf("   ");
		}
		if (statinfo)
			switch (i)
			{
			  case 0:
				printf("stardate      %.2f", Now.date);
				break;
			  case 1:
				printf("condition     %s", Color[Ship.cond]);
				if (Ship.cloaked)
					printf(", CLOAKED");
				break;
			  case 2:
				printf("position      %d,%d/%d,%d",Ship.quadx, Ship.quady, Ship.sectx, Ship.secty);
				break;
			  case 3:
				printf("warp factor   %.1f", Ship.warp);
				break;
			  case 4:
				printf("total energy  %d", Ship.energy);
				break;
			  case 5:
				printf("torpedoes     %d", Ship.torped);
				break;
			  case 6:
				s = "down";
				if (Ship.shldup)
					s = "up";
				if (damaged(SHIELD))
					s = "damaged";
				percent = 100.0 * Ship.shield / Param.shield;
				printf("shields       %s, %d%%", s, percent);
				break;
			  case 7:
				printf("Klingons left %d", Now.klings);
				break;
			  case 8:
				printf("time left     %.2f", Now.time);
				break;
			  case 9:
				printf("life support  ");
				if (damaged(LIFESUP))
				{
					printf("damaged, reserves = %.2f", Ship.reserves);
					break;
				}
				printf("active");
				break;
			}
		printf("\n");
	}
	if (f < 0)
	{
		printf("current crew  %d\n", Ship.crew);
		printf("brig space    %d\n", Ship.brigfree);
		printf("Klingon power %d\n", Param.klingpwr);
		p = &Lentab[Game.length - 1];
		if (Game.length > 2)
			p--;
		printf("Length, Skill %s%s, ", p->abrev, p->full);
		p = &Skitab[Game.skill - 1];
		printf("%s%s\n", p->abrev, p->full);
		return;
	}
	printf("  ");
	for (i = 0; i < NSECTS; i++)
		printf("%d ", i);
	printf("\n");

	if (q->qsystemname & Q_DISTRESSED)
		printf("Distressed ");
	if (q->qsystemname)
		printf("Starsystem %s\n", systemname(q));
}
