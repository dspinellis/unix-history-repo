/*
 * Copyright (c) 1985 Regents of the University of California.
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
static char sccsid[] = "@(#)execute.c	5.2 (Berkeley) 6/27/88";
#endif /* not lint */

/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 */

# include	"hunt.h"

# undef CTRL
# define	CTRL(x)	('x' & 037)

# ifdef MONITOR
/*
 * mon_execute:
 *	Execute a single monitor command
 */
mon_execute(pp)
register PLAYER	*pp;
{
	register char	ch;

	ch = pp->p_cbuf[pp->p_ncount++];
	switch (ch) {
	  case CTRL(L):
		sendcom(pp, REDRAW);
		break;
	  case 'q':
		(void) strcpy(pp->p_death, "| Quit |");
		break;
	}
}
# endif MONITOR

/*
 * execute:
 *	Execute a single command
 */
execute(pp)
register PLAYER	*pp;
{
	register char	ch;

	ch = pp->p_cbuf[pp->p_ncount++];

# ifdef	FLY
	if (pp->p_flying >= 0) {
		switch (ch) {
		  case CTRL(L):
			sendcom(pp, REDRAW);
			break;
		  case 'q':
			(void) strcpy(pp->p_death, "| Quit |");
			break;
		}
		return;
	}
# endif	FLY

	switch (ch) {
	  case CTRL(L):
		sendcom(pp, REDRAW);
		break;
	  case 'h':
		move(pp, LEFTS);
		break;
	  case 'H':
		face(pp, LEFTS);
		break;
	  case 'j':
		move(pp, BELOW);
		break;
	  case 'J':
		face(pp, BELOW);
		break;
	  case 'k':
		move(pp, ABOVE);
		break;
	  case 'K':
		face(pp, ABOVE);
		break;
	  case 'l':
		move(pp, RIGHT);
		break;
	  case 'L':
		face(pp, RIGHT);
		break;
	  case 'f':
		fire(pp, SHOT);
		break;
	  case 'g':
		fire(pp, GRENADE);
		break;
	  case 'F':
		fire(pp, SATCHEL);
		break;
	  case 'G':
		fire(pp, BOMB);
		break;
# ifdef	OOZE
	  case 'o':
		fire_slime(pp, SLIMEREQ);
		break;
	  case 'O':
		fire_slime(pp, SSLIMEREQ);
		break;
# endif	OOZE
	  case 's':
		scan(pp);
		break;
	  case 'c':
		cloak(pp);
		break;
	  case 'q':
		(void) strcpy(pp->p_death, "| Quit |");
		break;
	}
}

/*
 * move:
 *	Execute a move in the given direction
 */
move(pp, dir)
register PLAYER	*pp;
int		dir;
{
	register PLAYER	*newp;
	register int	x, y;
	register FLAG	moved;
	register BULLET	*bp;

	y = pp->p_y;
	x = pp->p_x;

	switch (dir) {
	  case LEFTS:
		x--;
		break;
	  case RIGHT:
		x++;
		break;
	  case ABOVE:
		y--;
		break;
	  case BELOW:
		y++;
		break;
	}

	moved = FALSE;
	switch (Maze[y][x]) {
	  case SPACE:
# ifdef RANDOM
	  case DOOR:
# endif RANDOM
		moved = TRUE;
		break;
	  case WALL1:
	  case WALL2:
	  case WALL3:
# ifdef REFLECT
	  case WALL4:
	  case WALL5:
# endif REFLECT
		break;
	  case MINE:
	  case GMINE:
		if (dir == pp->p_face)
			pickup(pp, y, x, 5, Maze[y][x]);
		else if (opposite(dir, pp->p_face))
			pickup(pp, y, x, 95, Maze[y][x]);
		else
			pickup(pp, y, x, 50, Maze[y][x]);
		Maze[y][x] = SPACE;
		moved = TRUE;
		break;
	  case SHOT:
	  case GRENADE:
	  case SATCHEL:
	  case BOMB:
		bp = is_bullet(y, x);
		if (bp != NULL)
			bp->b_expl = TRUE;
		Maze[y][x] = SPACE;
		moved = TRUE;
		break;
	  case LEFTS:
	  case RIGHT:
	  case ABOVE:
	  case BELOW:
# ifdef FLY
	  case FLYER:
# endif FLY
		if (dir != pp->p_face)
			sendcom(pp, BELL);
		else {
			newp = play_at(y, x);
			checkdam(newp, pp, pp->p_ident, STABDAM, KNIFE);
		}
		break;
	}
	if (moved) {
		if (pp->p_ncshot > 0)
			if (--pp->p_ncshot == MAXNCSHOT) {
				cgoto(pp, STAT_GUN_ROW, STAT_VALUE_COL);
				outstr(pp, " ok", 3);
			}
		if (pp->p_undershot) {
			fixshots(pp->p_y, pp->p_x, pp->p_over);
			pp->p_undershot = FALSE;
		}
		drawplayer(pp, FALSE);
		pp->p_over = Maze[y][x];
		pp->p_y = y;
		pp->p_x = x;
		drawplayer(pp, TRUE);
	}
}

/*
 * face:
 *	Change the direction the player is facing
 */
face(pp, dir)
register PLAYER	*pp;
register int	dir;
{
	if (pp->p_face != dir) {
		pp->p_face = dir;
		drawplayer(pp, TRUE);
	}
}

/*
 * fire:
 *	Fire a shot of the given type in the given direction
 */
fire(pp, type)
register PLAYER	*pp;
register char	type;
{
	register int	req_index;
	static int	req[4] = { BULREQ, GRENREQ, SATREQ, BOMBREQ };
	static int	shot_type[4] = { SHOT, GRENADE, SATCHEL, BOMB };

	if (pp == NULL)
		return;
	if (pp->p_ammo == 0) {
		message(pp, "No more charges.");
		return;
	}
	if (pp->p_ncshot > MAXNCSHOT)
		return;
	if (pp->p_ncshot++ == MAXNCSHOT) {
		cgoto(pp, STAT_GUN_ROW, STAT_VALUE_COL);
		outstr(pp, "   ", 3);
	}
	switch (type) {
	  case SHOT:
		req_index = 0;
		break;
	  case GRENADE:
		req_index = 1;
		break;
	  case SATCHEL:
		req_index = 2;
		break;
	  case BOMB:
		req_index = 3;
		break;
# ifdef DEBUG
	  default:
		message(pp, "What you do!!!");
		return;
# endif DEBUG
	}
	while (pp->p_ammo < req[req_index])
		req_index--;
	pp->p_ammo -= req[req_index];
	(void) sprintf(Buf, "%3d", pp->p_ammo);
	cgoto(pp, STAT_AMMO_ROW, STAT_VALUE_COL);
	outstr(pp, Buf, 3);

	add_shot(shot_type[req_index], pp->p_y, pp->p_x, pp->p_face,
		req[req_index], pp, FALSE, pp->p_face);
	pp->p_undershot = TRUE;

	/*
	 * Show the object to everyone
	 */
	showexpl(pp->p_y, pp->p_x, shot_type[req_index]);
	for (pp = Player; pp < End_player; pp++)
		sendcom(pp, REFRESH);
# ifdef MONITOR
	for (pp = Monitor; pp < End_monitor; pp++)
		sendcom(pp, REFRESH);
# endif MONITOR
}

# ifdef	OOZE
/*
 * fire_slime:
 *	Fire a slime shot in the given direction
 */
fire_slime(pp, req)
register PLAYER	*pp;
register int	req;
{
	if (pp == NULL)
		return;
	if (pp->p_ammo < req) {
		message(pp, "Not enough charges.");
		return;
	}
	if (pp->p_ncshot > MAXNCSHOT)
		return;
	if (pp->p_ncshot++ == MAXNCSHOT) {
		cgoto(pp, STAT_GUN_ROW, STAT_VALUE_COL);
		outstr(pp, "   ", 3);
	}
	pp->p_ammo -= req;
	(void) sprintf(Buf, "%3d", pp->p_ammo);
	cgoto(pp, STAT_AMMO_ROW, STAT_VALUE_COL);
	outstr(pp, Buf, 3);

	add_shot(SLIME, pp->p_y, pp->p_x, pp->p_face, req, pp, FALSE,
		pp->p_face);

	/*
	 * Show the object to everyone
	 */
	showexpl(pp->p_y, pp->p_x, SLIME);
	for (pp = Player; pp < End_player; pp++)
		sendcom(pp, REFRESH);
# ifdef MONITOR
	for (pp = Monitor; pp < End_monitor; pp++)
		sendcom(pp, REFRESH);
# endif MONITOR
}
# endif	OOZE

/*
 * create_shot:
 *	Create a shot with the given properties
 */
add_shot(type, y, x, face, charge, owner, expl, over)
int	type;
int	y, x;
char	face;
int	charge;
PLAYER	*owner;
int	expl;
char	over;
{
	register BULLET	*bp;

# ifdef CONSTANT_MOVE
	/*
	 * if there are no bullets in flight, set up the alarm
	 */

	if (Bullets == NULL)
		bul_alarm(1);
# endif CONSTANT_MOVE

	bp = create_shot(type, y, x, face, charge, owner,
		(owner == NULL) ? NULL : owner->p_ident, expl, over);
	bp->b_next = Bullets;
	Bullets = bp;
}

BULLET *
create_shot(type, y, x, face, charge, owner, score, expl, over)
int	type;
int	y, x;
char	face;
int	charge;
PLAYER	*owner;
IDENT	*score;
int	expl;
char	over;
{
	register BULLET	*bp;

	bp = (BULLET *) malloc(sizeof (BULLET));	/* NOSTRICT */
	if (bp == NULL) {
		if (owner != NULL)
			message(owner, "Out of memory");
		return NULL;
	}

	bp->b_face = face;
	bp->b_x = x;
	bp->b_y = y;
	bp->b_charge = charge;
	bp->b_owner = owner;
	bp->b_score = score;
	bp->b_type = type;
	bp->b_expl = expl;
	bp->b_over = over;
	bp->b_next = NULL;

	return bp;
}

/*
 * cloak:
 *	Turn on or increase length of a cloak
 */
cloak(pp)
register PLAYER	*pp;
{
	if (pp->p_ammo <= 0) {
		message(pp, "No more charges");
		return;
	}
	(void) sprintf(Buf, "%3d", --pp->p_ammo);
	cgoto(pp, STAT_AMMO_ROW, STAT_VALUE_COL);
	outstr(pp, Buf, 3);

	pp->p_cloak += CLOAKLEN;
	cgoto(pp, STAT_CLOAK_ROW, STAT_VALUE_COL);
	outstr(pp, " on", 3);

	if (pp->p_scan >= 0) {
		pp->p_scan = -1;
		cgoto(pp, STAT_SCAN_ROW, STAT_VALUE_COL);
		outstr(pp, "   ", 3);
	}

	showstat(pp);
}

/*
 * scan:
 *	Turn on or increase length of a scan
 */
scan(pp)
register PLAYER	*pp;
{
	if (pp->p_ammo <= 0) {
		message(pp, "No more charges");
		return;
	}
	(void) sprintf(Buf, "%3d", --pp->p_ammo);
	cgoto(pp, STAT_AMMO_ROW, STAT_VALUE_COL);
	outstr(pp, Buf, 3);

	pp->p_scan += SCANLEN;
	cgoto(pp, STAT_SCAN_ROW, STAT_VALUE_COL);
	outstr(pp, " on", 3);

	if (pp->p_cloak >= 0) {
		pp->p_cloak = -1;
		cgoto(pp, STAT_CLOAK_ROW, STAT_VALUE_COL);
		outstr(pp, "   ", 3);
	}

	showstat(pp);
}

/*
 * pickup:
 *	check whether the object blew up or whether he picked it up
 */
pickup(pp, y, x, prob, obj)
register PLAYER	*pp;
register int	y, x;
int		prob;
int		obj;
{
	register int	req;

	switch (obj) {
	  case MINE:
		req = BULREQ;
		break;
	  case GMINE:
		req = GRENREQ;
		break;
	  default:
		abort();
	}
	if (rand_num(100) < prob)
		add_shot(obj, y, x, LEFTS, req, (PLAYER *) NULL,
			TRUE, pp->p_face);
	else {
		pp->p_ammo += req;
		(void) sprintf(Buf, "%3d", pp->p_ammo);
		cgoto(pp, STAT_AMMO_ROW, STAT_VALUE_COL);
		outstr(pp, Buf, 3);
	}
}
