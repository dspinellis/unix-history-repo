/*
 * Copyright (c) 1983 Regents of the University of California.
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
static char sccsid[] = "@(#)pl_6.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include "player.h"

repair()
{
	char c;
	register char *repairs;
	register struct shipspecs *ptr = mc;
	register int count;

#define FIX(x, m) (m - ptr->x > count \
	? (ptr->x += count, count = 0) : (count -= m - ptr->x, ptr->x = m))

	if (repaired || loaded || fired || changed || turned()) {
		Signal("No hands free to repair", (struct ship *)0);
		return;
	}
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
	if (++*repairs >= 3) {
		count = 2;
		switch (c) {
		case 'h': {
			int max = ptr->guns/4;
			if (ptr->hull < max) {
				FIX(hull, max);
				Write(W_HULL, ms, 0, ptr->hull, 0, 0, 0);
			}
			break;
			}
		case 'g':
			if (ptr->gunL < ptr->gunR) {
				int max = ptr->guns/5 - ptr->carL;
				if (ptr->gunL < max) {
					FIX(gunL, max);
					Write(W_GUNL, ms, 0, ptr->gunL,
						ptr->carL, 0, 0);
				}
			} else {
				int max = ptr->guns/5 - ptr->carR;
				if (ptr->gunR < max) {
					FIX(gunR, max);
					Write(W_GUNR, ms, 0, ptr->gunR,
						ptr->carR, 0, 0);
				}
			}
			break;
		case 'r':
#define X 2
			if (ptr->rig4 >= 0 && ptr->rig4 < X) {
				FIX(rig4, X);
				Write(W_RIG4, ms, 0, ptr->rig4, 0, 0, 0);
			}
			if (count && ptr->rig3 < X) {
				FIX(rig3, X);
				Write(W_RIG3, ms, 0, ptr->rig3, 0, 0, 0);
			}
			if (count && ptr->rig2 < X) {
				FIX(rig2, X);
				Write(W_RIG2, ms, 0, ptr->rig2, 0, 0, 0);
			}
			if (count && ptr->rig1 < X) {
				FIX(rig1, X);
				Write(W_RIG1, ms, 0, ptr->rig1, 0, 0, 0);
			}
			break;
		}
		if (count == 2) {
			Signal("Repairs completed.", (struct ship *)0);
			*repairs = 2;
		} else {
			*repairs = 0;
			blockalarm();
			draw_stat();
			unblockalarm();
		}
	}
	blockalarm();
	draw_slot();
	unblockalarm();
	repaired = 1;
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
