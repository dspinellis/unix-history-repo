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
static char sccsid[] = "@(#)rnd_pos.c	5.2 (Berkeley) %G%";
#endif /* not lint */

# include	"robots.h"

# define	IS_SAME(p,y,x)	((p).y != -1 && (p).y == y && (p).x == x)

/*
 * rnd_pos:
 *	Pick a random, unoccupied position
 */
COORD *
rnd_pos()
{
	static COORD	pos;
	static int	call = 0;
	register int	i = 0;

	do {
		pos.y = rnd(Y_FIELDSIZE - 1) + 1;
		pos.x = rnd(X_FIELDSIZE - 1) + 1;
		refresh();
	} while (Field[pos.y][pos.x] != 0);
	call++;
	return &pos;
}

rnd(range)
int	range;
{
	unsigned int	rand();

	return rand() % range;
}
