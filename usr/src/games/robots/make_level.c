/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)make_level.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

# include	"robots.h"

/*
 * make_level:
 *	Make the current level
 */
make_level()
{
	register int	i;
	register COORD	*cp;
	register WINDOW	*wp;
	register int	x, *endp;

	reset_count();
	for (i = 1; i < Y_FIELDSIZE; i++)
		for (x = 1; x < X_FIELDSIZE; x++)
			if (Field[i][x] != 0)
				mvaddch(i, x, ' ');
	if (My_pos.y > 0)
		mvaddch(My_pos.y, My_pos.x, ' ');

	Waiting = FALSE;
	Wait_bonus = 0;
	leaveok(stdscr, FALSE);
	for (cp = Robots; cp < &Robots[MAXROBOTS]; cp++)
		cp->y = -1;
	My_pos.y = -1;

	bzero(Field, sizeof Field);
	Min.y = Y_FIELDSIZE;
	Min.x = X_FIELDSIZE;
	Max.y = 0;
	Max.x = 0;
	if ((i = Level * 10) > MAXROBOTS)
		i = MAXROBOTS;
	Num_robots = i;
	while (i-- > 0) {
		cp = rnd_pos();
		Robots[i] = *cp;
		Field[cp->y][cp->x]++;
		if (cp->y < Min.y)
			Min.y = cp->y;
		if (cp->x < Min.x)
			Min.x = cp->x;
		if (cp->y > Max.y)
			Max.y = cp->y;
		if (cp->x > Max.x)
			Max.x = cp->x;
	}
	My_pos = *rnd_pos();
	refresh();
}
