#ifndef lint
static	char *sccsid = "@(#)pl_3.c	1.1 83/07/20";
#endif

#include "player.h"

acceptcombat()
{
	int men = 0;
	int target, temp;
	int n, r;
	int index, rakehim, sternrake;
	int hhits = 0, ghits = 0, rhits = 0, chits = 0;
	int crew[3];
	int load;
	int guns, car, ready, shootat, hit;
	int roll;
	struct ship *closest;

	crew[0] = mc->crew1;
	crew[1] = mc->crew2;
	crew[2] = mc->crew3;
	for (n = 0; n < 3; n++) {
		if (mf->OBP[n].turnsent)
			men += mf->OBP[n].mensent;
	}
	for (n = 0; n < 3; n++) {
		if (mf->DBP[n].turnsent)
			men += mf->DBP[n].mensent;
	}
	if (men) {
		crew[0] = men/100 ? 0 : crew[0] != 0;
		crew[1] = (men%100)/10 ? 0 : crew[1] != 0;
		crew[2] = men%10 ? 0 : crew[2] != 0;
	}
	for (r = 0; r < 2; r++) {
		if (r) {
			ready = mf->readyR;
			load = mf->loadR;
			guns = mc->gunR;
			car = mc->carR;
		} else {
			ready = mf->readyL;
			load = mf->loadL;
			guns = mc->gunL;
			car = mc->carL;
		}
		if (!guns && !car || load == L_EMPTY || (ready & R_LOADED) == 0)
			goto cant;
		if (mf->struck || !crew[2])
			goto cant;
		closest = closestenemy(ms, (r ? 'r' : 'l'), 1);
		if (closest == 0)
			goto cant;
		if (closest->file->struck)
			goto cant;
		target = range(ms, closest);
		if (target > rangeofshot[load] || !guns && target >= 3)
			goto cant;
		Signal("%s (%c%c) within range of %s broadside.",
			closest, r ? "right" : "left");
		if (load > L_CHAIN && target < 6) {
			Signal("Aim for hull or rigging? ", (struct ship *)0);
			switch (sgetch(1)) {
			case 'r':
				shootat = RIGGING;
				break;
			case 'h':
				shootat = HULL;
				break;
			default:
				shootat = -1;
				Signal("'Avast there! Hold your fire.'",
					(struct ship *)0);
			}
		} else {
			Signal("Fire? ", (struct ship *)0);
			if (sgetch(1) == 'n') {
				shootat = -1;
				Signal("Belay that! Hold your fire.",
					(struct ship *)0);
			} else
				shootat = RIGGING;
		}
		if (shootat == -1)
			continue;
		fired = 1;
		rakehim = gunsbear(ms, closest) && !gunsbear(closest, ms);
		temp = portside(closest, ms, 1) - closest->file->dir + 1;
		if (temp < 1)
			temp += 8;
		else if (temp > 8)
			temp -= 8;
		sternrake = temp > 4 && temp < 6;
		if (rakehim)
			if (!sternrake)
				Signal("Raking the %s!", closest);
			else
				Signal("Stern Rake! %s splintering!", closest);
		index = guns;
		if (target < 3)
			index += car;
		index = (index - 1)/3;
		index = index > 8 ? 8 : index;
		if (!rakehim)
			hit = HDT[index][target-1];
		else
			hit = HDTrake[index][target-1];
		if (rakehim && sternrake)
			hit++;
		hit += QUAL[index][mc->qual-1];
		for (n = 0; n < 3 && mf->captured == 0; n++)
			if (!crew[n])
				if (index <= 5)
					hit--;
				else
					hit -= 2;
		if (ready & R_INITIAL)
			if (index <= 3)
				hit++;
			else
				hit += 2;
		if (mf->captured != 0)
			if (index <= 1)
				hit--;
			else
				hit -= 2;
		hit += AMMO[index][load - 1];
		if (((temp = mc->class) >= 5 || temp == 1) && windspeed == 5)
			hit--;
		if (windspeed == 6 && temp == 4)
			hit -= 2;
		if (windspeed == 6 && temp <= 3)
			hit--;
		if (hit >= 0) {
			roll = die();
			if (load == L_GRAPE)
				chits = hit;
			else {
				struct Tables *t;
				if (hit > 10)
					hit = 10;
				t = &(shootat == RIGGING ? RigTable : HullTable)
					[hit][roll-1];
				chits = t->C;
				rhits = t->R;
				hhits = t->H;
				ghits = t->G;
				if (closest->file->FS)
					rhits *= 2;
				if (load == L_CHAIN) {
					ghits = 0;
					hhits = 0;
				}
			}
			table(shootat, load, hit, closest, ms, roll);
		}
		Scroll2();
		(void) wprintw(scroll_w, "Damage inflicted on the %s:",
			closest->shipname);
		Scroll();
		(void) wprintw(scroll_w,
			"\t%d HULL, %d GUNS, %d CREW, %d RIGGING",
			hhits, ghits, chits, rhits);
		if (!r) {
			mf->loadL = L_EMPTY;
			mf->readyL = R_EMPTY;
		} else {
			mf->loadR = L_EMPTY;
			mf->readyR = R_EMPTY;
		}
		continue;
	cant:
		Signal("Unable to fire %s broadside",
			(struct ship *)0, r ? "right" : "left");
	}
}

grapungrap()
{
	register struct ship *sp;
	register int n, k;
	register struct snag *p;

	n = -1;
	foreachship(sp) {
		n++;
		if (sp == ms)
			continue;
		if (range(ms, sp) > 1 && !grappled2(ms, sp))
			continue;
		Signal("Attempt to grapple or ungrapple %s (%c%c): ", sp);
		switch (sgetch(1)) {
		case 'g':
			if (die() < 3
			    || ms->nationality == capship(sp)->nationality) {
				for (k = 0, p = mf->grapples;
				     k < NSHIP && p->turnfoul; k++, p++)
					;		/* XXX */
				if (k < NSHIP)
					Write(W_GRAP, ms, 0, k, turn, n, 0);
				for (k = 0, p = sp->file->grapples;
				     k < NSHIP && p->turnfoul; k++, p++)
					;		/* XXX */
				if (k < NSHIP)
					Write(W_GRAP, sp, 0,
						k, turn, player, 0);
				Signal("Attempt succeeds!", (struct ship *)0);
				makesignal(ms, "grappled with %s (%c%c)", sp);
			} else
				Signal("Attempt fails.", (struct ship *)0);
			break;
		case 'u':
			for (k = 0; k < NSHIP; k++) {
				if (!mf->grapples[k].turnfoul)
					continue;
				if (sp != mf->grapples[k].toship)
					continue;
				if (die() < 3 || ms->nationality
				    == capship(sp)->nationality) {
					cleangrapple(ms, sp, k);
					Signal("Attempt succeeds!",
						(struct ship *)0);
					makesignal(ms,
						"ungrappling with %s (%c%c)",
						sp);
				} else
					Signal("Attempt fails.",
						(struct ship *)0);
			}
		}
	}
}

unfoulplayer()
{
	register struct snag *s = mf->fouls;
	register struct ship *to;
	int n;

	for (n = 0; n < NSHIP; n++, s++) {
		if (s->turnfoul == 0)
			continue;
		to = s->toship;
		Signal("Attempt to unfoul with the %s (%c%c)? ", to);
		if (sgetch(1) == 'y') {
			if (die() < 3) {
				cleanfoul(ms, to, n);
				Signal("Attempt succeeds!",
					(struct ship *)0);
				makesignal(ms, "Unfouling %s (%c%c)", to);
			} else
				Signal("Attempt fails.", (struct ship *)0);
		}
	}
}

lookout()
{
	register struct ship *sp;
	char buf[3];
	register char c;

	Signal("What ship? ", (struct ship *)0);
	sgetstr(buf, sizeof buf);
	foreachship(sp) {
		c = *countryname[sp->nationality];
		if ((c == *buf || tolower(c) == *buf || colours(sp) == *buf)
		    && (sp->file->stern == buf[1] || sterncolour(sp) == buf[1]))
			break;
	}
	if (sp)
		eyeball(sp);
	else
		Signal("No such ship.", (struct ship *)0);
}

char *
saywhat(sp, flag)
register struct ship *sp;
char flag;
{
	if (sp->file->captain[0])
		return sp->file->captain;
	else if (sp->file->struck)
		return "(struck)";
	else if (sp->file->captured != 0)
		return "(captured)";
	else if (flag)
		return "(available)";
	else
		return "(computer)";
}

eyeball(ship)
register struct ship *ship;
{
	int i;

	if (ship == 0)
		Signal("No more ships left.", (struct ship *)0);
	else if (ship->shipdir) {
		Signal("Sail ho! (range %d, %s)",
			(struct ship *)0, range(ms, ship), saywhat(ship, 0));
		i = portside(ms, ship, 1) - mf->dir;
		if (i <= 0)
			i += 8;
		Signal("%s (%c%c) %s %s %s.",
			ship, countryname[ship->nationality],
			classname[ship->specs->class], directionname[i]);
	}
}
