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
	  case '1':
		fire(pp, 0);		/* SHOT */
		break;
	  case 'g':
	  case '2':
		fire(pp, 1);		/* GRENADE */
		break;
	  case 'F':
	  case '3':
		fire(pp, 2);		/* SATCHEL */
		break;
	  case 'G':
	  case '4':
		fire(pp, 3);		/* 7x7 BOMB */
		break;
	  case '5':
		fire(pp, 4);		/* 9x9 BOMB */
		break;
	  case '6':
		fire(pp, 5);		/* 11x11 BOMB */
		break;
	  case '7':
		fire(pp, 6);		/* 13x13 BOMB */
		break;
	  case '8':
		fire(pp, 7);		/* 15x15 BOMB */
		break;
	  case '9':
		fire(pp, 8);		/* 17x17 BOMB */
		break;
	  case '0':
		fire(pp, 9);		/* 19x19 BOMB */
		break;
	  case '@':
		fire(pp, 10);		/* 21x21 BOMB */
		break;
# ifdef	OOZE
	  case 'o':
		fire_slime(pp, 0);	/* SLIME */
		break;
	  case 'O':
		fire_slime(pp, 1);	/* SSLIME */
		break;
	  case 'p':
		fire_slime(pp, 2);
		break;
	  case 'P':
		fire_slime(pp, 3);
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
			pickup(pp, y, x, 2, Maze[y][x]);
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
# ifdef OOZE
	  case SLIME:
# endif OOZE
# ifdef DRONE
	  case DSHOT:
# endif DRONE
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
		if (dir != pp->p_face)
			sendcom(pp, BELL);
		else {
			newp = play_at(y, x);
			checkdam(newp, pp, pp->p_ident, STABDAM, KNIFE);
		}
		break;
# ifdef FLY
	  case FLYER:
		newp = play_at(y, x);
		message(newp, "Oooh, there's a short guy waving at you!");
		message(pp, "You couldn't quite reach him!");
		break;
# endif FLY
# ifdef BOOTS
	  case BOOT:
	  case BOOT_PAIR:
		if (Maze[y][x] == BOOT)
			pp->p_nboots++;
		else
			pp->p_nboots += 2;
		for (newp = Boot; newp < &Boot[NBOOTS]; newp++) {
			if (newp->p_flying < 0)
				continue;
			if (newp->p_y == y && newp->p_x == x) {
				newp->p_flying = -1;
				if (newp->p_undershot)
					fixshots(y, x, newp->p_over);
			}
		}
		if (pp->p_nboots == 2)
			message(pp, "Wow!  A pair of boots!");
		else
			message(pp, "You can hobble around on one boot.");
		Maze[y][x] = SPACE;
		moved = TRUE;
		break;
# endif BOOTS
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
fire(pp, req_index)
register PLAYER	*pp;
register int	req_index;
{
	if (pp == NULL)
		return;
# ifdef DEBUG
	if (req_index < 0 || req_index >= MAXBOMB)
		message(pp, "What you do?");
# endif DEBUG
	while (req_index >= 0 && pp->p_ammo < shot_req[req_index])
		req_index--;
	if (req_index < 0) {
		message(pp, "Not enough charges.");
		return;
	}
	if (pp->p_ncshot > MAXNCSHOT)
		return;
	if (pp->p_ncshot++ == MAXNCSHOT) {
		cgoto(pp, STAT_GUN_ROW, STAT_VALUE_COL);
		outstr(pp, "   ", 3);
	}
	pp->p_ammo -= shot_req[req_index];
	(void) sprintf(Buf, "%3d", pp->p_ammo);
	cgoto(pp, STAT_AMMO_ROW, STAT_VALUE_COL);
	outstr(pp, Buf, 3);

	add_shot(shot_type[req_index], pp->p_y, pp->p_x, pp->p_face,
		shot_req[req_index], pp, FALSE, pp->p_face);
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
fire_slime(pp, req_index)
register PLAYER	*pp;
register int	req_index;
{
	if (pp == NULL)
		return;
# ifdef DEBUG
	if (req_index < 0 || req_index >= MAXSLIME)
		message(pp, "What you do?");
# endif DEBUG
	while (req_index >= 0 && pp->p_ammo < slime_req[req_index])
		req_index--;
	if (req_index < 0) {
		message(pp, "Not enough charges.");
		return;
	}
	if (pp->p_ncshot > MAXNCSHOT)
		return;
	if (pp->p_ncshot++ == MAXNCSHOT) {
		cgoto(pp, STAT_GUN_ROW, STAT_VALUE_COL);
		outstr(pp, "   ", 3);
	}
	pp->p_ammo -= slime_req[req_index];
	(void) sprintf(Buf, "%3d", pp->p_ammo);
	cgoto(pp, STAT_AMMO_ROW, STAT_VALUE_COL);
	outstr(pp, Buf, 3);

	add_shot(SLIME, pp->p_y, pp->p_x, pp->p_face,
		slime_req[req_index] * SLIME_FACTOR, pp, FALSE, pp->p_face);
	pp->p_undershot = TRUE;

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
 * add_shot:
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
	register int	size;

	switch (type) {
	  case SHOT:
	  case MINE:
		size = 1;
		break;
	  case GRENADE:
	  case GMINE:
		size = 2;
		break;
	  case SATCHEL:
		size = 3;
		break;
	  case BOMB:
		for (size = 3; size < MAXBOMB; size++)
			if (shot_req[size] >= charge)
				break;
		size++;
		break;
	  default:
		size = 0;
		break;
	}

	bp = create_shot(type, y, x, face, charge, size, owner,
		(owner == NULL) ? NULL : owner->p_ident, expl, over);
	bp->b_next = Bullets;
	Bullets = bp;
}

BULLET *
create_shot(type, y, x, face, charge, size, owner, score, expl, over)
int	type;
int	y, x;
char	face;
int	charge;
int	size;
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
	bp->b_size = size;
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
# ifdef BOOTS
	if (pp->p_nboots > 0) {
		message(pp, "Boots are too noisy to cloak!");
		return;
	}
# endif BOOTS
	(void) sprintf(Buf, "%3d", --pp->p_ammo);
	cgoto(pp, STAT_AMMO_ROW, STAT_VALUE_COL);
	outstr(pp, Buf, 3);

	pp->p_cloak += CLOAKLEN;

	if (pp->p_scan >= 0)
		pp->p_scan = -1;

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

	if (pp->p_cloak >= 0)
		pp->p_cloak = -1;

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
