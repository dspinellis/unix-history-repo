/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)end.c	5.2 (Berkeley) %G%";
#endif /* not lint */

# include	"mille.h"

/*
 * @(#)end.c	1.1 (Berkeley) 4/1/82
 */

/*
 *	print out the score as if it was final, and add the totals for
 * the end-of-games points to the user who deserves it (if any).
 */
finalscore(pp)
reg PLAY	*pp; {

	reg int		temp, tot, num;

	if (pp->was_finished == Finished)
		return;

	pp->was_finished = Finished;
	num = pp - Player;
	temp = num * 6 + 21 + 1;
	for (tot = 5; tot <= 9; tot++)
		mvaddstr(tot, temp, "  0");
	if (pp->mileage == End) {
		mvaddstr(5, temp, "40");
		tot = SC_TRIP;
		if (pp->nummiles[C_200] == 0) {
			mvaddstr(6, temp, "30");
			tot = SC_TRIP + SC_SAFE;
		}
		if (Topcard <= Deck) {
			mvaddstr(7, temp, "30");
			tot += SC_DELAY;
		}
		if (End == 1000) {
			mvaddstr(8, temp, "20");
			tot += SC_EXTENSION;
		}
		if (Player[other(num)].mileage == 0) {
			mvaddstr(9, temp, "50");
			tot += SC_SHUT_OUT;
		}
		pp->total += tot;
		pp->hand_tot += tot;
	}
}

# ifdef EXTRAP
static int	Last_tot[2];	/* last tot used for extrapolate	*/

/*
 *	print out the score as if it was final, and add the totals for
 * the end-of-games points to the user who deserves it (if any).
 */
extrapolate(pp)
reg PLAY	*pp; {

	reg int		x, num, tot, count;

	num = pp - Player;
	tot += SC_TRIP + SC_DELAY + SC_EXT;
	x = num * 6 + 21 + 3;
	for (tot = 5; tot <= 9; tot++)
		mvaddch(tot, x, '0');
	x -= 2;
	pp = &Player[other(num)];
	for (count = 0, tot = 0; tot < NUM_SAFE; tot++)
		if (pp->safety[tot] != S_PLAYED)
			count += SC_SAFE;
	mvprintw(3, x, "%3d", count);
	tot += count;
	if (count == 400) {
		mvaddstr(4, x, "30");
		tot += SC_ALL_SAFE;
	}
	pp = &Player[num];
	for (count = 0, tot = 0; tot < NUM_SAFE; tot++)
		if (pp->safety[tot] != S_PLAYED)
			count += SC_COUP / 10;
	mvprintw(4, x - 1, "%3d", count);
	tot += count;
	tot += 1000 - pp->mileage;
	mvaddstr(5, x, "40");
	mvaddstr(7, x, "30");
	mvaddstr(8, x, "20");
	if (pp->nummiles[C_200] == 0) {
		mvaddstr(6, x, "30");
		tot = SC_TRIP + SC_SAFE;
	}
	if (Player[other(num)].mileage == 0) {
		mvaddstr(9, x, "50");
		tot += SC_SHUT_OUT;
	}
	pp->total += tot;
	pp->hand_tot += tot;
	Last_tot[num] = tot;
}

undoex() {

	reg PLAY	*pp;
	reg int		i;

	i = 0;
	for (pp = Player; pp < &Player[2]; pp++) {
		pp->total -= Last_tot[i];
		pp->hand_tot -= Last_tot[i++];
	}
}
# endif

