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

/*
 * showexpl:
 *	Show the explosions as they currently are
 */
showexpl(y, x, type)
register int	y, x;
char		type;
{
	register PLAYER	*pp;
	register EXPL	*ep;

	if (y < 0 || y >= HEIGHT)
		return;
	if (x < 0 || x >= WIDTH)
		return;
	ep = (EXPL *) malloc(sizeof (EXPL));	/* NOSTRICT */
	ep->e_y = y;
	ep->e_x = x;
	ep->e_char = type;
	ep->e_next = NULL;
	if (Last_expl == NULL)
		Expl[0] = ep;
	else
		Last_expl->e_next = ep;
	Last_expl = ep;
	for (pp = Player; pp < End_player; pp++) {
		if (pp->p_maze[y][x] == type)
			continue;
		pp->p_maze[y][x] = type;
		cgoto(pp, y, x);
		outch(pp, type);
	}
# ifdef MONITOR
	for (pp = Monitor; pp < End_monitor; pp++) {
		if (pp->p_maze[y][x] == type)
			continue;
		pp->p_maze[y][x] = type;
		cgoto(pp, y, x);
		outch(pp, type);
	}
# endif MONITOR
	switch (Maze[y][x]) {
	  case WALL1:
	  case WALL2:
	  case WALL3:
# ifdef RANDOM
	  case DOOR:
# endif RANDOM
# ifdef REFLECT
	  case WALL4:
	  case WALL5:
# endif REFLECT
		if (y >= UBOUND && y < DBOUND && x >= LBOUND && x < RBOUND)
			remove_wall(y, x);
		break;
	}
}

/*
 * rollexpl:
 *	Roll the explosions over, so the next one in the list is at the
 *	top
 */
rollexpl()
{
	register EXPL	*ep;
	register PLAYER	*pp;
	register int	y, x;
	register char	c;
	register EXPL	*nextep;

	for (ep = Expl[EXPLEN - 1]; ep != NULL; ep = nextep) {
		nextep = ep->e_next;
		y = ep->e_y;
		x = ep->e_x;
		if (y < UBOUND || y >= DBOUND || x < LBOUND || x >= RBOUND)
			c = Maze[y][x];
		else
			c = SPACE;
		for (pp = Player; pp < End_player; pp++)
			if (pp->p_maze[y][x] == ep->e_char) {
				pp->p_maze[y][x] = c;
				cgoto(pp, y, x);
				outch(pp, c);
			}
# ifdef MONITOR
		for (pp = Monitor; pp < End_monitor; pp++)
			check(pp, y, x);
# endif MONITOR
		free((char *) ep);
	}
	for (x = EXPLEN - 1; x > 0; x--)
		Expl[x] = Expl[x - 1];
	Last_expl = Expl[0] = NULL;
}

/* There's about 700 walls in the initial maze.  So we pick a number
 * that keeps the maze relatively full. */
# define MAXREMOVE	40

static	REGEN	removed[MAXREMOVE];
static	REGEN	*rem_index = removed;

/*
 * remove_wall - add a location where the wall was blown away.
 *		 if there is no space left over, put the a wall at
 *		 the location currently pointed at.
 */
remove_wall(y, x)
int	y, x;
{
	register REGEN	*r;
# if defined(MONITOR) || defined(FLY)
	register PLAYER	*pp;
# endif MONITOR || FLY
# ifdef	FLY
	register char	save_char;
# endif	FLY

	r = rem_index;
	while (r->r_y != 0) {
# ifdef FLY
		switch (Maze[r->r_y][r->r_x]) {
		  case SPACE:
		  case LEFTS:
		  case RIGHT:
		  case ABOVE:
		  case BELOW:
		  case FLYER:
			save_char = Maze[r->r_y][r->r_x];
			goto found;
		}
# else FLY
		if (Maze[r->r_y][r->r_x] == SPACE)
			break;
# endif FLY
		if (++r >= &removed[MAXREMOVE])
			r = removed;
	}

found:
	if (r->r_y != 0) {
		/* Slot being used, put back this wall */
# ifdef FLY
		if (save_char == SPACE)
			Maze[r->r_y][r->r_x] = Orig_maze[r->r_y][r->r_x];
		else {
			pp = play_at(r->r_y, r->r_x);
			if (pp->p_flying >= 0)
				pp->p_flying += rand_num(10);
			else {
				pp->p_flying = rand_num(20);
				pp->p_flyx = 2 * rand_num(6) - 5;
				pp->p_flyy = 2 * rand_num(6) - 5;
			}
			pp->p_over = Orig_maze[r->r_y][r->r_x];
			pp->p_face = FLYER;
			Maze[r->r_y][r->r_x] = FLYER;
			showexpl(r->r_y, r->r_x, FLYER);
		}
# else FLY
		Maze[r->r_y][r->r_x] = Orig_maze[r->r_y][r->r_x];
# endif FLY
# ifdef RANDOM
		if (rand_num(100) == 0)
			Maze[r->r_y][r->r_x] = DOOR;
# endif RANDOM
# ifdef REFLECT
		if (rand_num(100) == 0)		/* one percent of the time */
			Maze[r->r_y][r->r_x] = WALL4;
# endif REFLECT
# ifdef MONITOR
		for (pp = Monitor; pp < End_monitor; pp++)
			check(pp, r->r_y, r->r_x);
# endif MONITOR
	}

	r->r_y = y;
	r->r_x = x;
	if (++r >= &removed[MAXREMOVE])
		rem_index = removed;
	else
		rem_index = r;

	Maze[y][x] = SPACE;
# ifdef MONITOR
	for (pp = Monitor; pp < End_monitor; pp++)
		check(pp, y, x);
# endif MONITOR
}

/*
 * clearwalls:
 *	Clear out the walls array
 */
clearwalls()
{
	register REGEN	*rp;

	for (rp = removed; rp < &removed[MAXREMOVE]; rp++)
		rp->r_y = 0;
	rem_index = removed;
}
