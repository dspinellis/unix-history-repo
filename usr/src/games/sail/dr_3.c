#ifndef lint
static	char *sccsid = "@(#)dr_3.c	2.3 83/12/12";
#endif

#include "driver.h"

moveall()		/* move all comp ships */
{
	register struct ship *sp, *sq;		/* r11, r10 */
	register int n;				/* r9 */
	register int k, l, m;			/* r8, r7, r6, */
	int row[NSHIP], col[NSHIP], dir[NSHIP], drift[NSHIP];
	char moved[NSHIP];

	/*
	 * first try to create moves for OUR ships
	 */
	foreachship(sp) {
		struct ship *closest;
		int ma, ta;
		char af;

		if (sp->file->captain[0] || sp->file->dir == 0)
			continue;
		if (!sp->file->struck && windspeed && !snagged(sp)
		    && sp->specs->crew3) {
			ta = maxturns(sp, &af);
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
	 * checking for collisions and snags at each step.
	 * The old positions are saved in row[], col[], dir[].
	 * At the end, we compare and write out the changes.
	 */
	n = 0;
	foreachship(sp) {
		if (snagged(sp))
			strcpy(sp->file->last, "d");
		else
			if (*sp->file->last != 'd')
				strcat(sp->file->last, "d");
		row[n] = sp->file->row;
		col[n] = sp->file->col;
		dir[n] = sp->file->dir;
		drift[n] = sp->file->drift;
		moved[n] = 0;
		n++;
	}
	/*
	 * Now resolve collisions.
	 * This is the tough part.
	 */
	for (k = 0; stillmoving(k); k++) {
		/*
		 * Step once.
		 * And propagate the nulls at the end of sp->file->last.
		 */
		n = 0;
		foreachship(sp) {
			if (!sp->file->last[k])
				sp->file->last[k+1] = '\0';
			else if (sp->file->dir)
				step(sp->file->last[k], sp, &moved[n]);
			n++;
		}
		/*
		 * The real stuff.
		 */
		n = 0;
		foreachship(sp) {
			if (sp->file->dir == 0 || isolated(sp))
				goto cont1;
			l = 0;
			foreachship(sq) {
				char snap = 0;

				if (sp == sq)
					goto cont2;
				if (sq->file->dir == 0)
					goto cont2;
				if (!push(sp, sq))
					goto cont2;
				if (snagged2(sp, sq) && range(sp, sq) > 1)
					snap++;
				if (!range(sp, sq) && !fouled2(sp, sq)) {
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
					snap++;
				}
				if (snap) {
					sp->file->last[k + 1] = 0;
					sq->file->last[k + 1] = 0;
					sq->file->row = sp->file->row - 1;
					if (sp->file->dir == 1
					    || sp->file->dir == 5)
						sq->file->col =
							sp->file->col - 1;
					else
						sq->file->col = sp->file->col;
					sq->file->dir = sp->file->dir;
				}
			cont2:
				l++;
			}
		cont1:
			n++;
		}
	}
	/*
	 * Clear old moves.  And write out new pos.
	 */
	n = 0;
	foreachship(sp) {
		if (sp->file->dir != 0) {
			*sp->file->last = 0;
			if (row[n] != sp->file->row)
				Write(W_SHIPROW, sp, 0, sp->file->row, 0, 0, 0);
			if (col[n] != sp->file->col)
				Write(W_SHIPCOL, sp, 0, sp->file->col, 0, 0, 0);
			if (dir[n] != sp->file->dir)
				Write(W_SHIPDIR, sp, 0, sp->file->dir, 0, 0, 0);
			if (drift[n] != sp->file->drift)
				Write(W_DRIFT, sp, 0, sp->file->drift, 0, 0, 0);
		}
		n++;
	}
}

stillmoving(k)
register int k;
{
	register struct ship *sp;

	foreachship(sp)
		if (sp->file->last[k])
			return 1;
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

push(from, to)
register struct ship *from, *to;
{
	register int bs, sb;

	sb = to->specs->guns;
	bs = from->specs->guns;
	if (sb > bs)
		return 1;
	if (sb < bs)
		return 0;
	return from < to;
}

step(com, sp, moved)
char com;
register struct ship *sp;
char *moved;
{
	register int dist;

	switch (com) {
	case 'r':
		if (++sp->file->dir == 9)
			sp->file->dir = 1;
		break;
	case 'l':
		if (--sp->file->dir == 0)
			sp->file->dir = 8;
		break;
	case '0': case '1': case '2': case '3':
	case '4': case '5': case '6': case '7':
		if (sp->file->dir % 2 == 0)
			dist = dtab[com - '0'];
		else
			dist = com - '0';
		sp->file->row -= dr[sp->file->dir] * dist;
		sp->file->col -= dc[sp->file->dir] * dist;
		*moved = 1;
		break;
	case 'b':
		break;
	case 'd':
		if (!*moved) {
			if (++sp->file->drift > 2 &&
			    (sp->specs->class >= 3 && !snagged(sp)
			     || (turn & 1) == 0)) {
				sp->file->row -= dr[winddir];
				sp->file->col -= dc[winddir];
			}
		} else
			sp->file->drift = 0;
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
