/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)pl_2.c	5.1 (Berkeley) 5/29/85";
#endif not lint

#include "player.h"

play()
{
	register struct ship *sp;

	for (;;) {
		switch (sgetch("~\b", (struct ship *)0, 0)) {
		case 'm':
			acceptmove();
			break;
		case 's':
			acceptsignal();
			break;
		case 'g':
			grapungrap();
			break;
		case 'u':
			unfoulplayer();
			break;
		case 'v':
			Signal("%s", (struct ship *)0, version);
			break;
		case 'b':
			acceptboard();
			break;
		case 'f':
			acceptcombat();
			break;
		case 'l':
			loadplayer();
			break;
		case 'c':
			changesail();
			break;
		case 'r':
			repair();
			break;
		case 'B':
			Signal("'Hands to stations!'", (struct ship *)0);
			unboard(ms, ms, 1);	/* cancel DBP's */
			unboard(ms, ms, 0);	/* cancel offense */
			break;
		case '\f':
			centerview();
			blockalarm();
			draw_board();
			draw_screen();
			unblockalarm();
			break;
		case 'L':
			mf->loadL = L_EMPTY;
			mf->loadR = L_EMPTY;
			mf->readyL = R_EMPTY;
			mf->readyR = R_EMPTY;
			Signal("Broadsides unloaded", (struct ship *)0);
			break;
		case 'q':
			Signal("Type 'Q' to quit", (struct ship *)0);
			break;
		case 'Q':
			leave(LEAVE_QUIT);
			break;
		case 'I':
			foreachship(sp)
				if (sp != ms)
					eyeball(sp);
			break;
		case 'i':
			if ((sp = closestenemy(ms, 0, 1)) == 0)
				Signal("No more ships left.");
			else
				eyeball(sp);
			break;
		case 'C':
			centerview();
			blockalarm();
			draw_view();
			unblockalarm();
			break;
		case 'U':
			upview();
			blockalarm();
			draw_view();
			unblockalarm();
			break;
		case 'D':
		case 'N':
			downview();
			blockalarm();
			draw_view();
			unblockalarm();
			break;
		case 'H':
			leftview();
			blockalarm();
			draw_view();
			unblockalarm();
			break;
		case 'J':
			rightview();
			blockalarm();
			draw_view();
			unblockalarm();
			break;
		case 'F':
			lookout();
			break;
		case 'S':
			dont_adjust = !dont_adjust;
			blockalarm();
			draw_turn();
			unblockalarm();
			break;
		}
	}
}
