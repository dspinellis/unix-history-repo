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
static char sccsid[] = "@(#)ram.c	5.3 (Berkeley) %G%";
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
