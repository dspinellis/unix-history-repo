#ifndef lint
static	char *sccsid = "@(#)dr_3.c	1.4 83/07/20";
#endif

#include "driver.h"

moveall()		/* move all comp ships */
{
	register struct ship *sp, *sq;		/* r11, r10 */
	register int n;				/* r9 */
	struct ship *closest;
	register int k, l, m, ma;		/* r8, r7, r6, */
	int ta, af;
	int row[NSHIP], col[NSHIP], dir[NSHIP], r1, r2, c1, c2, d1, d2;
	char clast[NSHIP][sizeof SHIP(0)->file->last];

	/*
	 * first try to create moves for OUR ships
	 */
	foreachship(sp) {
		if (sp->file->captain[0] || sp->shipdir == 0)
			continue;
		if (!sp->file->struck && windspeed && !snagged(sp)
		    && sp->specs->crew3) {
			ta = maxturns(sp);
			af = ta & 0100000;
			ta &= 077777;
			ma = maxmove(sp, sp->file->dir, 0);
			closest = closestenemy(sp, 0, 0);
			if (closest == 0)
				*sp->file->last = '\0';
			else
				closeon(sp, closest, sp->file->last,
					ta, ma, af);
		} else
			*sp->file->last = '\0';
	}
	/*
	 * Then execute the moves for ALL ships (dead ones too),
	 * saving old positions in row[], col[], dir[],
	 * and the moves in clase[][].
	 * The new positions are written out.
	 */
	n = 0;
	foreachship(sp) {
		if (snagged(sp))
			clast[n][0] = '\0';
		else
			(void) strcpy(clast[n], sp->file->last);
		row[n] = sp->file->row;
		col[n] = sp->file->col;
		dir[n] = sp->file->dir;
		moveship(sp, clast[n]);
		n++;
	}
	/*
	 * Now resolve collisions.
	 * This is the tough part.
	 */
	for (k = 0; stillmoving(clast, k); k++) {
		/*
		 * Step once.
		 * And propagate the nulls at the end of clast[].
		 */
		n = 0;
		foreachship(sp) {
			if (dir[n])
				step(clast[n][k], sp, row+n, col+n, dir+n);
			if (!clast[n][k])
				clast[n][k+1] = '\0';
			n++;
		}
		/*
		 * The real stuff.
		 */
		n = 0;
		foreachship(sp) {
			if ((d1 = sp->file->dir) == 0 || isolated(sp))
				continue;
			r1 = sp->file->row;
			c1 = sp->file->col;
			sp->file->dir = dir[n];
			sp->file->row = row[n];
			sp->file->col = col[n];
			l = 0;
			foreachship(sq) {
				if ((d2 = sq->file->dir) == 0 || sp == sq)
					continue;
				r2 = sq->file->row;
				c2 = sq->file->col;
				sq->file->dir = dir[l];
				sq->file->row = row[l];
				sq->file->col = col[l];
				if (grappled2(sp, sq)
				    && push(sp, sq) && range(sp, sq) > 1) {
					Write(W_SHIPROW, sq, 0,
						sp->file->row - 1, 0, 0, 0);
					if (sp->file->dir == 1
					    || sp->file->dir == 5) /* XXX */
						Write(W_SHIPCOL, sq, 0,
							sp->file->col - 1,
							0, 0, 0);
					else
						Write(W_SHIPCOL, sq, 0,
							sp->file->col, 0, 0, 0);
					Write(W_SHIPDIR, sq, 0,
						sp->file->dir, 0, 0, 0);
				}
				if (!range(sp, sq) && !fouled2(sp, sq)
				    && push(sp, sq)) {
					makesignal(sp,
						"collision with %s (%c%c)", sq);
					if (die() < 4) {
						makesignal(sp,
							"fouled with %s (%c%c)",
							sq);
						for (m = 0; m < NSHIP && sp->file->fouls[m].turnfoul; m++)
							;
						if (m < NSHIP)
							Write(W_FOUL, sp, 0,
								m, turn, l, 0);
						for (m = 0; m < NSHIP && sq->file->fouls[m].turnfoul; m++)
							;
						if (m < NSHIP)
							Write(W_FOUL, sq, 0,
								m, turn, n, 0);
					}
					clast[n][k+1] = '\0';
					sp->file->row = r2;
					sp->file->col = c2;
					sp->file->dir = d2;
					moveship(sp, clast[n]);
					Write(W_SHIPROW, sq, 0,
						sp->file->row-1, 0, 0, 0);
					if (sp->file->dir == 1
					    || sp->file->dir == 5)
						Write(W_SHIPCOL, sq, 0,
							sp->file->col-1, 0, 0, 0);
					else
						Write(W_SHIPCOL, sq, 0,
							sp->file->col, 0, 0, 0);
					Write(W_SHIPDIR, sq, 0,
						sp->file->dir, 0, 0, 0);
					Write(W_DRIFT, sq, 0, 0, 0, 0, 0);
					Write(W_DRIFT, sp, 0, 0, 0, 0, 0);
				} else {
					sq->file->row = r2;
					sq->file->col = c2;
					sq->file->dir = d2;
				}
				l++;
			}
			sp->file->row = r1;
			sp->file->col = c1;
			sp->file->dir = d1;
			n++;
		}
	}
	/*
	 * Clear old moves.
	 */
	foreachship(sp)
		sp->file->last[0] = 0;
}

stillmoving(last, k)
char last[][sizeof SHIP(0)->file->last];	/* how's that for portability */
register int k;
{
	register struct ship *sp;
	register char (*p)[sizeof *last];	/* and this? */

	p = last;
	foreachship(sp) {
		if ((*p)[k])
			return 1;
		p++;
	}
	return 0;
}

isolated(ship)
register struct ship *ship;
{
	register struct ship *sp;

	foreachship(sp) {
		if (ship != sp && range(ship, sp) <= 10)
			return 0;
	}
	return 1;
}

/* XXX */
push(from, to)
register struct ship *from, *to;
{
	int bow1r, bow1c, bow2r, bow2c, stern1r, stern1c, stern2r, stern2c;
	register int bs, sb;

	stern1r = bow1r = from->file->row;
	stern1c = bow1c = from->file->col;
	stern2r = bow2r = to->file->row;
	stern2c = bow2c = to->file->col;
	stern2r += dr[to->file->dir];
	stern2c += dc[to->file->dir];
	bs = bow1r - stern2r + bow1c - stern2c;
	sb = stern1r - bow2r + stern1c - bow2c;
	if (!bs)
		return 1;
	stern1r += dr[from->file->dir];
	stern1c += dc[from->file->dir];
	if(!sb)
		return 0;
	sb = to->specs->class;
	bs = from->specs->class;
	if (sb > bs)
		return 1;
	if (sb < bs)
		return 0;
	return from < to;
}

step(com, ship, row, col, dir)
register struct ship *ship;
register int *row, *col, *dir;
char com;
{
	register int dist;

	switch (com) {
	case 'r':
		if (++*dir == 9)
			*dir = 1;
		break;
	case 'l':
		if (--*dir == 0)
			*dir = 8;
		break;
	case '0': case '1': case '2': case '3':
	case '4': case '5': case '6': case '7':
		if (*dir % 2 == 0)
			dist = dtab[com - '0'];
		else
			dist = com - '0';
		*row -= dr[*dir] * dist;
		*col -= dc[*dir] * dist;
		break;
	case 'b':
		break;
	case 'd':
		if (ship->specs->class >= 3 && !snagged(ship)
		    || turn % 2 == 0) {
			*row -= dr[winddir];
			*col -= dc[winddir];
		}
		break;
	}
}

sendbp(from, to, sections, isdefense)
register struct ship *from, *to;
int sections;
char isdefense;
{
	int n;
	register struct BP *bp;

	bp = isdefense ? from->file->DBP : from->file->OBP;
	for (n = 0; n < 3 && bp[n].turnsent; n++)
		;
	if (n < 3 && sections) {
		Write(isdefense ? W_DBP : W_OBP, from, 0,
			turn, to-SHIP(0), sections, 0);
		if (isdefense)
			makesignal(from, "repelling boarders",
				(struct ship *)0);
		else
			makesignal(from, "boarding the %s (%c%c)", to);
	}
}

toughmelee(ship, to, isdefense, count)
register struct ship *ship, *to;
int isdefense, count;
{
	register struct BP *bp;
	register obp = 0;
	int n, OBP = 0, DBP = 0, dbp = 0;
	int qual;

	qual = ship->specs->qual;
	bp = isdefense ? ship->file->DBP : ship->file->OBP;
	for (n = 0; n < NBP; n++, bp++) {
		if (bp->turnsent && (to == bp->toship || isdefense)) {
			obp += bp->mensent / 100
				? ship->specs->crew1 * qual : 0;
			obp += (bp->mensent % 100)/10
				? ship->specs->crew2 * qual : 0;
			obp += bp->mensent % 10
				? ship->specs->crew3 * qual : 0;
		}
	}
	if (count || isdefense)
		return obp;
	OBP = toughmelee(to, ship, 0, count + 1);
	dbp = toughmelee(ship, to, 1, count + 1);
	DBP = toughmelee(to, ship, 1, count + 1);
	if (OBP > obp + 10 || OBP + DBP >= obp + dbp + 10)
		return 1;
	else
		return 0;
}

reload()
{
	register struct ship *sp;

	foreachship(sp) {
		sp->file->loadwith = 0;
	}
}

checksails()
{
	register struct ship *sp;
	register int rig, full; 
	struct ship *close;

	foreachship(sp) {
		if (sp->file->captain[0] != 0)
			continue;
		rig = sp->specs->rig1;
		if (windspeed == 6 || windspeed == 5 && sp->specs->class > 4)
			rig = 0;
		if (rig && sp->specs->crew3) {
			close = closestenemy(sp, 0, 0);
			if (close != 0) {
				if (range(sp, close) > 9)
					full = 1;
				else
					full = 0;
			} else 
				full = 0;
		} else
			full = 0;
		if ((sp->file->FS != 0) != full)
			Write(W_FS, sp, 0, full, 0, 0, 0);
	}
}
