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
static char sccsid[] = "@(#)score.c	5.3 (Berkeley) 6/18/88";
#endif /* not lint */

# include	"trek.h"
# include	"getpar.h"

/*
**  PRINT OUT THE CURRENT SCORE
*/

long score()
{
	register int		u;
	register int		t;
	long			s;
	double			r;
	extern struct cvntab	Skitab[];

	printf("\n*** Your score:\n");
	s = t = Param.klingpwr / 4 * (u = Game.killk);
	if (t != 0)
		printf("%d Klingons killed\t\t\t%6d\n", u, t);
	r = Now.date - Param.date;
	if (r < 1.0)
		r = 1.0;
	r = Game.killk / r;
	s += (t = 400 * r);
	if (t != 0)
		printf("Kill rate %.2f Klingons/stardate  \t%6d\n", r, t);
	r = Now.klings;
	r /= Game.killk + 1;
	s += (t = -400 * r);
	if (t != 0)
		printf("Penalty for %d klingons remaining\t%6d\n", Now.klings, t);
	if (Move.endgame > 0)
	{
		s += (t = 100 * (u = Game.skill));
		printf("Bonus for winning a %s%s game\t\t%6d\n", Skitab[u - 1].abrev, Skitab[u - 1].full, t);
	}
	if (Game.killed)
	{
		s -= 500;
		printf("Penalty for getting killed\t\t  -500\n");
	}
	s += (t = -100 * (u = Game.killb));
	if (t != 0)
		printf("%d starbases killed\t\t\t%6d\n", u, t);
	s += (t = -100 * (u = Game.helps));
	if (t != 0)
		printf("%d calls for help\t\t\t%6d\n", u, t);
	s += (t = -5 * (u = Game.kills));
	if (t != 0)
		printf("%d stars destroyed\t\t\t%6d\n", u, t);
	s += (t = -150 * (u = Game.killinhab));
	if (t != 0)
		printf("%d inhabited starsystems destroyed\t%6d\n", u, t);
	if (Ship.ship != ENTERPRISE)
	{
		s -= 200;
		printf("penalty for abandoning ship\t\t  -200\n");
	}
	s += (t = 3 * (u = Game.captives));
	if (t != 0)
		printf("%d Klingons captured\t\t\t%6d\n", u, t);
	s += (t = -(u = Game.deaths));
	if (t != 0)
		printf("%d casualties\t\t\t\t%6d\n", u, t);
	printf("\n***  TOTAL\t\t\t%14ld\n", s);
	return (s);
}
