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
static char sccsid[] = "@(#)ram.c	5.5 (Berkeley) 6/1/90";
#endif /* not lint */

# include	"trek.h"

/*
**  RAM SOME OBJECT
**
**	You have run into some sort of object.  It may be a Klingon,
**	a star, or a starbase.  If you run into a star, you are really
**	stupid, because there is no hope for you.
**
**	If you run into something else, you destroy that object.  You
**	also rack up incredible damages.
*/

ram(ix, iy)
int	ix, iy;
{
	register int		i;
	register char		c;

	printf("\07RED ALERT\07: collision imminent\n");
	c = Sect[ix][iy];
	switch (c)
	{

	  case KLINGON:
		printf("%s rams Klingon at %d,%d\n", Ship.shipname, ix, iy);
		killk(ix, iy);
		break;

	  case STAR:
	  case INHABIT:
		printf("Yeoman Rand: Captain, isn't it getting hot in here?\n");
		sleep(2);
		printf("Spock: Hull temperature approaching 550 Degrees Kelvin.\n");
		lose(L_STAR);

	  case BASE:
		printf("You ran into the starbase at %d,%d\n", ix, iy);
		killb(Ship.quadx, Ship.quady);
		/* don't penalize the captain if it wasn't his fault */
		if (!damaged(SINS))
			Game.killb += 1;
		break;
	}
	sleep(2);
	printf("%s heavily damaged\n", Ship.shipname);

	/* select the number of deaths to occur */
	i = 10 + ranf(20 * Game.skill);
	Game.deaths += i;
	Ship.crew -= i;
	printf("McCoy: Take it easy Jim; we had %d casualties.\n", i);

	/* damage devices with an 80% probability */
	for (i = 0; i < NDEV; i++)
	{
		if (ranf(100) < 20)
			continue;
		damage(i, (2.5 * (franf() + franf()) + 1.0) * Param.damfac[i]);
	}

	/* no chance that your shields remained up in all that */
	Ship.shldup = 0;
}
