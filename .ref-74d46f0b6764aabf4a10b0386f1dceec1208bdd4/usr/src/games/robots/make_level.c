/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)make_level.c	5.4 (Berkeley) %G%";
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
