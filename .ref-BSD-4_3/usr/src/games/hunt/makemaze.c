/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 *
 *  Copyright (c) 1985 Regents of the University of California.
 *  All rights reserved.  The Berkeley software License Agreement
 *  specifies the terms and conditions for redistribution.
 */

# include	"hunt.h"

# define	ISCLEAR(y,x)	(Maze[y][x] == SPACE)
# define	ODD(n)		((n) & 01)

makemaze()
{
	register char	*sp;
	register int	y, x;

	/*
	 * fill maze with walls
	 */
	sp = &Maze[0][0];
	while (sp < &Maze[HEIGHT - 1][WIDTH])
		*sp++ = DOOR;

	y = rand_num(DBOUND - UBOUND) + UBOUND;
	x = rand_num(RBOUND - LBOUND) + LBOUND;
	dig(y, x);				/* Dig out the maze */
	remap();
}

# define	NPERM	24
# define	NDIR	4

int	dirs[NPERM][NDIR] = {
		{0,1,2,3},	{3,0,1,2},	{0,2,3,1},	{0,3,2,1},
		{1,0,2,3},	{2,3,0,1},	{0,2,1,3},	{2,3,1,0},
		{1,0,3,2},	{1,2,0,3},	{3,1,2,0},	{2,0,3,1},
		{1,3,0,2},	{0,3,1,2},	{1,3,2,0},	{2,0,1,3},
		{0,1,3,2},	{3,1,0,2},	{2,1,0,3},	{1,2,3,0},
		{2,1,3,0},	{3,0,2,1},	{3,2,0,1},	{3,2,1,0}
	};

int	incr[NDIR][2] = {
		{0, 1}, {1, 0}, {0, -1}, {-1, 0}
	};

dig(y, x)
int	y, x;
{
	register int	*dp;
	register int	*ip;
	register int	ny, nx;
	register int	*endp;

	Maze[y][x] = SPACE;			/* Clear this spot */
	dp = dirs[rand_num(NPERM)];
	endp = &dp[NDIR];
	while (dp < endp) {
		ip = &incr[*dp++][0];
		ny = y + *ip++;
		nx = x + *ip;
		if (candig(ny, nx))
			dig(ny, nx);
	}
}

/*
 * candig:
 *	Is it legal to clear this spot?
 */
candig(y, x)
register int	y, x;
{
	register int	i;

	if (ODD(x) && ODD(y))
		return FALSE;		/* can't touch ODD spots */

	if (y < UBOUND || y >= DBOUND)
		return FALSE;		/* Beyond vertical bounds, NO */
	if (x < LBOUND || x >= RBOUND)
		return FALSE;		/* Beyond horizontal bounds, NO */

	if (ISCLEAR(y, x))
		return FALSE;		/* Already clear, NO */

	i = ISCLEAR(y, x + 1);
	i += ISCLEAR(y, x - 1);
	if (i > 1)
		return FALSE;		/* Introduces cycle, NO */
	i += ISCLEAR(y + 1, x);
	if (i > 1)
		return FALSE;		/* Introduces cycle, NO */
	i += ISCLEAR(y - 1, x);
	if (i > 1)
		return FALSE;		/* Introduces cycle, NO */

	return TRUE;			/* OK */
}

remap()
{
	register int	y, x;
	register char	*sp;
	register int	stat;

	for (y = 0; y < HEIGHT; y++)
		for (x = 0; x < WIDTH; x++) {
			sp = &Maze[y][x];
			if (*sp == SPACE)
				continue;
			stat = 0;
			if (y - 1 >= 0 && Maze[y - 1][x] != SPACE)
				stat |= NORTH;
			if (y + 1 < HEIGHT && Maze[y + 1][x] != SPACE)
				stat |= SOUTH;
			if (x + 1 < WIDTH && Maze[y][x + 1] != SPACE)
				stat |= EAST;
			if (x - 1 >= 0 && Maze[y][x - 1] != SPACE)
				stat |= WEST;
			switch (stat) {
			  case WEST | EAST:
				*sp = WALL1;
				break;
			  case NORTH | SOUTH:
				*sp = WALL2;
				break;
			  case 0:
# ifdef RANDOM
				*sp = DOOR;
# endif RANDOM
# ifdef REFLECT
				*sp = rand_num(2) ? WALL4 : WALL5;
# endif REFLECT
				break;
			  default:
				*sp = WALL3;
				break;
			}
		}
	bcopy((char *) Maze, (char *) Orig_maze, sizeof Maze);
}
