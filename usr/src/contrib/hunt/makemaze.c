/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
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

	x = rand_num(WIDTH / 2) * 2 + 1;
	y = rand_num(HEIGHT / 2) * 2 + 1;
	dig_maze(x, y);
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

dig_maze(x, y)
int	x, y;
{
	register int	tx, ty;
	register int	i, j;
	int		order[4];
#define	MNORTH	0x1
#define	MSOUTH	0x2
#define	MEAST	0x4
#define	MWEST	0x8

	Maze[y][x] = SPACE;
	order[0] = MNORTH;
	for (i = 1; i < 4; i++) {
		j = rand_num(i + 1);
		order[i] = order[j];
		order[j] = 0x1 << i;
	}
	for (i = 0; i < 4; i++) {
		switch (order[i]) {
		  case MNORTH:
			tx = x;
			ty = y - 2;
			break;
		  case MSOUTH:
			tx = x;
			ty = y + 2;
			break;
		  case MEAST:
			tx = x + 2;
			ty = y;
			break;
		  case MWEST:
			tx = x - 2;
			ty = y;
			break;
		}
		if (tx < 0 || ty < 0 || tx >= WIDTH || ty >= HEIGHT)
			continue;
		if (Maze[ty][tx] == SPACE)
			continue;
		Maze[(y + ty) / 2][(x + tx) / 2] = SPACE;
		dig_maze(tx, ty);
	}
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
			  case EAST:
			  case WEST:
				*sp = WALL1;
				break;
			  case NORTH | SOUTH:
			  case NORTH:
			  case SOUTH:
				*sp = WALL2;
				break;
			  case 0:
# ifdef RANDOM
				*sp = DOOR;
# endif
# ifdef REFLECT
				*sp = rand_num(2) ? WALL4 : WALL5;
# endif
				break;
			  default:
				*sp = WALL3;
				break;
			}
		}
	memcpy(Orig_maze, Maze, sizeof Maze);
}
