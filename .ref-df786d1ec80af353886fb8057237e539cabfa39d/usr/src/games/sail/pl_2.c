#ifndef lint
static	char *sccsid = "@(#)pl_2.c	1.3 83/07/20";
#endif

#include "player.h"

#define turnfirst(buf)  (*buf == 'r' || *buf == 'l')

lost()
{
	if (mf->struck)
		leave(LEAVE_QUIT);
	if (mf->captured != 0)
		leave(LEAVE_CAPTURED);
	if (windspeed == 7)
		leave(LEAVE_HURRICAN);
	if (mf->FS && (!mc->rig1 || windspeed == 6))
		Write(W_FS, ms, 0, 0, 0, 0, 0);
}

acceptmove(ma, ta, af)
int ma, ta, af;
{
	int moved = 0;
	int vma, dir;
	char buf[60], last = '\0';
	register char *p;

	if (*movebuf) {
		Signal("Already moved.", (struct ship *)0);
		return;
	}
	Signal("move (%d,%c%d): ", (struct ship *)0, ma, af ? '\'' : ' ', ta);
	sgetstr(buf, sizeof buf);
	buf[sizeof movebuf - 1] = '\0';
	dir = mf->dir;
	vma = ma;
	for (p = buf; *p; p++)
		switch (*p) {
		case 'l':
			dir -= 2;
		case 'r':
			if (++dir == 0)
				dir = 8;
			else if (dir == 9)
				dir = 1;
			if (last == 't') {
				Signal("Ship can't turn that fast.",
					(struct ship *)0);
				*p-- = '\0';
			}
			last = 't';
			ma--;
			ta--;
			vma = min(ma, maxmove(ms, dir, 0));
			if (ta < 0 && moved || vma < 0 && moved)
				*p-- = '\0';
			break;
		case 'b':
			ma--;
			vma--;
			last = 'b';
			if (ta < 0 && moved || vma < 0 && moved)
				*p-- = '\0';
			break;
		case '0':
		case 'd':
			*p-- = '\0';
			break;
		case '\n':
			*p-- = '\0';
			break;
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7':
			if (last == '0') {
				Signal("Can't move that fast.",
					(struct ship *)0);
				*p-- = '\0';
			}
			last = '0';
			moved = 1;
			ma -= *p - '0';
			vma -= *p - '0';
			if (ta < 0 && moved || vma < 0 && moved)
				*p-- = '\0';
			break;
		default:
			if (!isspace(*p)) {
				Signal("Input error.", (struct ship *)0);
				*p-- = '\0';
			}
		}
	if (ta < 0 && moved || vma < 0 && moved
	    || af && turnfirst(buf) && moved) {
		Signal("Movement error.", (struct ship *)0);
		if (ta < 0 && moved) {
			if (mf->FS == 1) {
				Write(W_FS, ms, 0, 0, 0, 0, 0);
				Signal("No hands to set full sails.",
					(struct ship *)0);
			}
		} else if (ma >= 0)
			buf[1] = '\0';
	}
	if (af && !moved) {
		if (mf->FS == 1) {
			Write(W_FS, ms, 0, 0, 0, 0, 0);
			Signal("No hands to set full sails.",
				(struct ship *)0);
		}
	}
	if (*buf)
		(void) strcpy(movebuf, buf);
	else
		(void) strcpy(movebuf, "d");
	Write(W_LAST, ms, 1, (int)movebuf, 0, 0, 0);
	Signal("Helm: %s.", (struct ship *)0, movebuf);
}

doboarding()
{
	register struct ship *sp;
	register int n;
	int crew[3];
	int men = 0;

	crew[0] = mc->crew1;
	crew[1] = mc->crew2;
	crew[2] = mc->crew3;
	for (n=0; n < 3; n++) {
		if (mf->OBP[n].turnsent)
			    men += mf->OBP[n].turnsent;
	}
	for (n=0; n < 3; n++) {
		if (mf->DBP[n].turnsent)
			    men += mf->DBP[n].turnsent;
	}
	if (men) {
		crew[0] = men/100 ? 0 : crew[0] != 0;
		crew[1] = (men%100)/10 ? 0 : crew[1] != 0;
		crew[2] = men%10 ? 0 : crew[2] != 0;
	} else {
		crew[0] = crew[0] != 0;
		crew[1] = crew[1] != 0;
		crew[2] = crew[2] != 0;
	}
	foreachship(sp) {
		if (sp == ms || sp->file->dir == 0 || range(ms, sp) > 0)
			continue;
		if (ms->nationality == capship(sp)->nationality)
			continue;
		if (meleeing(ms, sp) && crew[2]) {
			Signal("How many more to board the %s (%c%c)? ", sp);
			parties(crew, sp, 0);
		} else if (grappled2(ms, sp) && crew[2]) {
			Signal("Crew sections to board the %s (%c%c) (3 max) ?",
				sp);
			parties(crew, sp, 0);
		}
	}
	if (crew[2]) {
		Signal("How many sections to repel boarders? ",
			(struct ship *)0);
		parties(crew, ms, 1);
	}
}

parties(crew, to, isdefense)
register struct ship *to;
int crew[3];
char isdefense;
{
	register int k, j, men; 
	struct BP *ptr;
	int buf;
	int temp[3];

	for (k = 0; k < 3; k++)
		temp[k] = crew[k];
	buf = sgetch(1);
	if (isdigit(buf)) {
		ptr = isdefense ? to->file->DBP : to->file->OBP; 
		for (j = 0; j < NBP && ptr[j].turnsent; j++)
			;
		if (!ptr[j].turnsent && buf > '0') {
			men = 0;
			for (k=0; k < 3 && buf > '0'; k++) {
				men += crew[k] * power(10, 2-k);
				crew[k] = 0;
				if (men)
					buf -= 1;
			}
			if (buf > '0')
				Signal("Sending all crew sections.",
					(struct ship *)0);
			Write(isdefense ? W_DBP : W_OBP, ms, 0,
				j, turn, to-SHIP(0), men);
			if (isdefense) {
				(void) wmove(slot_w, 2, 0);
				for (k=0; k < NBP; k++)
					if (temp[k] && !crew[k])
						(void) waddch(slot_w, k + '1');
					else
						(void) wmove(slot_w, 2, 1 + k);
				(void) mvwaddstr(slot_w, 3, 0, "DBP");
				makesignal(ms, "repelling boarders",
					(struct ship *)0);
			} else {
				(void) wmove(slot_w, 0, 0);
				for (k=0; k < NBP; k++)
					if (temp[k] && !crew[k])
						(void) waddch(slot_w, k + '1');
					else
						(void) wmove(slot_w, 0, 1 + k);
				(void) mvwaddstr(slot_w, 1, 0, "OBP");
				makesignal(ms, "boarding the %s (%c%c)", to);
			}
			(void) wrefresh(slot_w);
		} else
			Signal("Sending no crew sections.", (struct ship *)0);
	}
}

power(base, exp)
int base, exp;
{
	switch (exp) {
		case 0:
			return 1;
		case 1:
			return base;
		case 2:
			return base * base;
	}
	return 0;
}

repair()
{
	int buf;
	int *repairs;
	struct shipspecs *ptr;

	if (repaired || loaded || fired || changed || turned()) {
		Signal("No hands free to repair", (struct ship *)0);
		return;
	}
	ptr = mc;
	Signal("Repair (hull, guns, rigging)? ", (struct ship *)0);
	buf = sgetch(1);
	switch (buf) {
		case 'h':
			repairs = &mf->RH;
			break;
		case 'g':
			repairs = &mf->RG;
			break;
		case 'r':
			repairs = &mf->RR;
			break;
		default:
			Signal("Avast heaving!", (struct ship *)0);
			return;
	}
	repaired = 1;
	if (++*repairs >= 3) {
		*repairs = 0;
		switch (buf) {
		case 'h':
			if (ptr->hull < ptr->guns/4)
				Write(W_HULL, ms, 0,
					ptr->hull + 2, 0, 0, 0);
			else
				buf = 0;
			break;
		case 'g':
			if (ptr->gunL < ptr->gunR) {
				if (ptr->gunL + ptr->carL < ptr->guns/5)
					Write(W_GUNL, ms, 0,
						ptr->gunL + 2, ptr->carL, 0, 0);
				else
					buf = 0;
			} else
				if (ptr->gunR + ptr->carR < ptr->guns/5)
					Write(W_GUNR, ms, 0,
						ptr->gunR + 2, ptr->carR, 0, 0);
				else
					buf = 0;
			break;
		case 'r':
			if (!ptr->rig4)
				Write(W_RIG4, ms, 0,
					ptr->rig4 + 2, 0, 0, 0);
			else if (!ptr->rig3)
				Write(W_RIG3, ms, 0, 2, 0, 0, 0);
			else if (!ptr->rig2)
				Write(W_RIG2, ms, 0, 2, 0, 0, 0);
			else if (ptr->rig1 < 4)
				Write(W_RIG1, ms, 0, 2, 0, 0, 0);
			else
				buf = 0;
			break;
		}
		if (!buf)
			Signal("Repairs completed.", (struct ship *)0);
	}
}

turned()
{
	register char *p;

	for (p = movebuf; *p; p++)
		if (*p == 'r' || *p == 'l')
			return 1;
	return 0;
}

loadplayer()
{
	int buf;
	register loadL, loadR, ready, load;

	if (!mc->crew3) {
		Signal("Out of crew", (struct ship *)0);
		return;
	}
	loadL = mf->loadL;
	loadR = mf->loadR;
	if (!loadL && !loadR) {
		Signal("Load which broadside (left or right)? ",
			(struct ship *)0);
		buf = sgetch(1);
		if (buf == 'r')
			loadL = 1;
		else
			loadR = 1;
	}
	if (!loadL && loadR || loadL && !loadR) {
		Signal("Reload with (round, double, chain, grape)? ",
			(struct ship *)0);
		buf = sgetch(1);
		switch (buf) {
		case 'r':
			load = L_ROUND;
			ready = 0;
			break;
		case 'd':
			load = L_DOUBLE;
			ready = R_DOUBLE;
			break;
		case 'c':
			load = L_CHAIN;
			ready = 0;
			break;
		case 'g':
			load = L_GRAPE;
			ready = 0;
			break;
		default:
			Signal("Broadside not loaded.",
				(struct ship *)0);
			return;
		}
		if (!loadR) {
			mf->loadR = load;
			mf->readyR = ready|R_LOADING;
		} else {
			mf->loadL = load;
			mf->readyL = ready|R_LOADING;
		}
		loaded = 1;
	}
}
