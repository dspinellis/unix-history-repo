#ifndef lint
static	char *sccsid = "@(#)pl_6.c	2.1 83/10/31";
#endif

#include "player.h"

repair()
{
	char c;
	char *repairs;
	struct shipspecs *ptr;

	if (repaired || loaded || fired || changed || turned()) {
		Signal("No hands free to repair", (struct ship *)0);
		return;
	}
	ptr = mc;
	c = sgetch("Repair (hull, guns, rigging)? ", (struct ship *)0, 1);
	switch (c) {
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
		switch (c) {
		case 'h':
			if (ptr->hull < ptr->guns/4)
				Write(W_HULL, ms, 0,
					ptr->hull + 2, 0, 0, 0);
			else
				c = 0;
			break;
		case 'g':
			if (ptr->gunL < ptr->gunR) {
				if (ptr->gunL + ptr->carL < ptr->guns/5)
					Write(W_GUNL, ms, 0,
						ptr->gunL + 2, ptr->carL, 0, 0);
				else
					c = 0;
			} else
				if (ptr->gunR + ptr->carR < ptr->guns/5)
					Write(W_GUNR, ms, 0,
						ptr->gunR + 2, ptr->carR, 0, 0);
				else
					c = 0;
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
				c = 0;
			break;
		}
		if (!c) {
			Signal("Repairs completed.", (struct ship *)0);
			*repairs = 2;
		} else {
			*repairs = 0;
			draw_stat();
		}
	}
	draw_slot();
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
	char c;
	register loadL, loadR, ready, load;

	if (!mc->crew3) {
		Signal("Out of crew", (struct ship *)0);
		return;
	}
	loadL = mf->loadL;
	loadR = mf->loadR;
	if (!loadL && !loadR) {
		c = sgetch("Load which broadside (left or right)? ",
			(struct ship *)0, 1);
		if (c == 'r')
			loadL = 1;
		else
			loadR = 1;
	}
	if (!loadL && loadR || loadL && !loadR) {
		c = sgetch("Reload with (round, double, chain, grape)? ",
			(struct ship *)0, 1);
		switch (c) {
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
