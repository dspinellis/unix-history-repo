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

drawmaze(pp)
register PLAYER	*pp;
{
	register int	x;
	register char	*sp;
	register int	y;
	register char	*endp;

	clrscr(pp);
	outstr(pp, pp->p_maze[0], WIDTH);
	for (y = 1; y < HEIGHT - 1; y++) {
		endp = &pp->p_maze[y][WIDTH];
		for (x = 0, sp = pp->p_maze[y]; sp < endp; x++, sp++)
			if (*sp != SPACE) {
				cgoto(pp, y, x);
				if (pp->p_x == x && pp->p_y == y)
					outch(pp, translate(*sp));
				else if (isplayer(*sp))
					outch(pp, player_sym(pp, y, x));
				else
					outch(pp, *sp);
			}
	}
	cgoto(pp, HEIGHT - 1, 0);
	outstr(pp, pp->p_maze[HEIGHT - 1], WIDTH);
	drawstatus(pp);
}

/*
 * drawstatus - put up the status lines (this assumes the screen
 *		size is 80x24 with the maze being 64x24)
 */
drawstatus(pp)
register PLAYER	*pp;
{
	register int	i;
	register PLAYER	*np;

	cgoto(pp, STAT_AMMO_ROW, STAT_LABEL_COL);
	outstr(pp, "Ammo:", 5);
	(void) sprintf(Buf, "%3d", pp->p_ammo);
	cgoto(pp, STAT_AMMO_ROW, STAT_VALUE_COL);
	outstr(pp, Buf, 3);

	cgoto(pp, STAT_GUN_ROW, STAT_LABEL_COL);
	outstr(pp, "Gun:", 4);
	cgoto(pp, STAT_GUN_ROW, STAT_VALUE_COL);
	outstr(pp, (pp->p_ncshot < MAXNCSHOT) ? " ok" : "   ", 3);

	cgoto(pp, STAT_DAM_ROW, STAT_LABEL_COL);
	outstr(pp, "Damage:", 7);
	(void) sprintf(Buf, "%2d/%2d", pp->p_damage, pp->p_damcap);
	cgoto(pp, STAT_DAM_ROW, STAT_VALUE_COL);
	outstr(pp, Buf, 5);

	cgoto(pp, STAT_KILL_ROW, STAT_LABEL_COL);
	outstr(pp, "Kills:", 6);
	(void) sprintf(Buf, "%3d", (pp->p_damcap - MAXDAM) / 2);
	cgoto(pp, STAT_KILL_ROW, STAT_VALUE_COL);
	outstr(pp, Buf, 3);

	cgoto(pp, STAT_PLAY_ROW, STAT_LABEL_COL);
	outstr(pp, "Player:", 7);
	for (i = STAT_PLAY_ROW + 1, np = Player; np < End_player; np++) {
		(void) sprintf(Buf, "%5.2f%c%-10.10s %c", np->p_ident->i_score,
			stat_char(np), np->p_ident->i_name,
			np->p_ident->i_team);
		cgoto(pp, i++, STAT_NAME_COL);
		outstr(pp, Buf, STAT_NAME_LEN);
	}

# ifdef MONITOR
	cgoto(pp, STAT_MON_ROW, STAT_LABEL_COL);
	outstr(pp, "Monitor:", 8);
	for (i = STAT_MON_ROW + 1, np = Monitor; np < End_monitor; np++) {
		(void) sprintf(Buf, "%5.5s %-10.10s %c", " ",
			np->p_ident->i_name, np->p_ident->i_team);
		cgoto(pp, i++, STAT_NAME_COL);
		outstr(pp, Buf, STAT_NAME_LEN);
	}
# endif MONITOR
}

look(pp)
register PLAYER	*pp;
{
	register int	x, y;

	x = pp->p_x;
	y = pp->p_y;

	check(pp, y - 1, x - 1);
	check(pp, y - 1, x    );
	check(pp, y - 1, x + 1);
	check(pp, y    , x - 1);
	check(pp, y    , x    );
	check(pp, y    , x + 1);
	check(pp, y + 1, x - 1);
	check(pp, y + 1, x    );
	check(pp, y + 1, x + 1);

	switch (pp->p_face) {
	  case LEFTS:
		see(pp, LEFTS);
		see(pp, ABOVE);
		see(pp, BELOW);
		break;
	  case RIGHT:
		see(pp, RIGHT);
		see(pp, ABOVE);
		see(pp, BELOW);
		break;
	  case ABOVE:
		see(pp, ABOVE);
		see(pp, LEFTS);
		see(pp, RIGHT);
		break;
	  case BELOW:
		see(pp, BELOW);
		see(pp, LEFTS);
		see(pp, RIGHT);
		break;
# ifdef FLY
	  case FLYER:
		break;
# endif FLY
	}
	cgoto(pp, y, x);
}

see(pp, face)
register PLAYER	*pp;
int		face;
{
	register char	*sp;
	register int	y, x, i, cnt;

	x = pp->p_x;
	y = pp->p_y;

	switch (face) {
	  case LEFTS:
		sp = &Maze[y][x];
		for (i = 0; See_over[*--sp]; i++)
			continue;

		if (i == 0)
			break;

		cnt = i;
		x = pp->p_x - 1;
		--y;
		while (i--)
			check(pp, y, --x);
		i = cnt;
		x = pp->p_x - 1;
		++y;
		while (i--)
			check(pp, y, --x);
		i = cnt;
		x = pp->p_x - 1;
		++y;
		while (i--)
			check(pp, y, --x);
		break;
	  case RIGHT:
		sp = &Maze[y][++x];
		for (i = 0; See_over[*sp++]; i++)
			continue;

		if (i == 0)
			break;

		cnt = i;
		x = pp->p_x + 1;
		--y;
		while (i--)
			check(pp, y, ++x);
		i = cnt;
		x = pp->p_x + 1;
		++y;
		while (i--)
			check(pp, y, ++x);
		i = cnt;
		x = pp->p_x + 1;
		++y;
		while (i--)
			check(pp, y, ++x);
		break;
	  case ABOVE:
		sp = &Maze[--y][x];
		if (!See_over[*sp])
			break;
		do {
			--y;
			sp -= sizeof Maze[0];
			check(pp, y, x - 1);
			check(pp, y, x    );
			check(pp, y, x + 1);
		} while (See_over[*sp]);
		break;
	  case BELOW:
		sp = &Maze[++y][x];
		if (!See_over[*sp])
			break;
		do {
			y++;
			sp += sizeof Maze[0];
			check(pp, y, x - 1);
			check(pp, y, x    );
			check(pp, y, x + 1);
		} while (See_over[*sp]);
		break;
	}
}

check(pp, y, x)
PLAYER	*pp;
int	y, x;
{
	register int	index;
	register int	ch;
	register PLAYER	*rpp;

	index = y * sizeof Maze[0] + x;
	ch = ((char *) Maze)[index];
	if (ch != ((char *) pp->p_maze)[index]) {
		rpp = pp;
		cgoto(rpp, y, x);
		if (x == rpp->p_x && y == rpp->p_y)
			outch(rpp, translate(ch));
		else if (isplayer(ch))
			outch(rpp, player_sym(rpp, y, x));
		else
			outch(rpp, ch);
		((char *) rpp->p_maze)[index] = ch;
	}
}

/*
 * showstat
 *	Update the status of players
 */
showstat(pp)
register PLAYER	*pp;
{
	register PLAYER	*np;
	register int	y;
	register char	c;

	y = STAT_PLAY_ROW + 1 + (pp - Player);
	c = stat_char(pp);
# ifdef MONITOR
	for (np = Monitor; np < End_monitor; np++) {
		cgoto(np, y, STAT_SCAN_COL);
		outch(np, c);
	}
# endif MONITOR
	for (np = Player; np < End_player; np++) {
		cgoto(np, y, STAT_SCAN_COL);
		outch(np, c);
	}
}

/*
 * drawplayer:
 *	Draw the player on the screen and show him to everyone who's scanning
 *	unless he is cloaked.
 */
drawplayer(pp, draw)
PLAYER	*pp;
FLAG	draw;
{
	register PLAYER	*newp;
	register int	x, y;

	x = pp->p_x;
	y = pp->p_y;
	Maze[y][x] = draw ? pp->p_face : pp->p_over;

# ifdef MONITOR
	for (newp = Monitor; newp < End_monitor; newp++)
		check(newp, y, x);
# endif MONITOR

	for (newp = Player; newp < End_player; newp++) {
		if (!draw || newp == pp) {
			check(newp, y, x);
			continue;
		}
		if (newp->p_scan == 0) {
			newp->p_scan--;
			showstat(newp);
		}
		else if (newp->p_scan > 0) {
			if (pp->p_cloak < 0)
				check(newp, y, x);
			newp->p_scan--;
		}
	}
	if (!draw || pp->p_cloak < 0)
		return;
	if (pp->p_cloak-- == 0)
		showstat(pp);
}

message(pp, s)
register PLAYER	*pp;
char		*s;
{
	cgoto(pp, HEIGHT, 0);
	outstr(pp, s, strlen(s));
	ce(pp);
}

/*
 * translate:
 *	Turn a character into the right direction character if we are
 *	looking at the current player.
 */
translate(ch)
char	ch;
{
	switch (ch) {
	  case LEFTS:
		return '<';
	  case RIGHT:
		return '>';
	  case ABOVE:
		return '^';
	  case BELOW:
		return 'v';
	}
	return ch;
}

/*
 * player_sym:
 *	Return the player symbol
 */
player_sym(pp, y, x)
PLAYER	*pp;
int	y, x;
{
	register PLAYER	*npp;

	npp = play_at(y, x);
	if (npp->p_ident->i_team == ' ')
		return Maze[y][x];
#ifdef MONITOR
	if (pp->p_ident->i_team == '*')
		return npp->p_ident->i_team;
#endif
	if (pp->p_ident->i_team != npp->p_ident->i_team)
		return Maze[y][x];
	return pp->p_ident->i_team;
}
