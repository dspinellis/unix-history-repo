/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwflush.c	3.13 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"
#include <sys/signal.h>

wwflush()
{
	register row, col;

	if ((row = wwcursorrow) < 0)
		row = 0;
	else if (row >= wwnrow)
		row = wwnrow - 1;
	if ((col = wwcursorcol) < 0)
		col = 0;
	else if (col >= wwncol)
		col = wwncol - 1;
	xxmove(row, col);
	if (wwdocheckpoint) {
		xxflush(0);
		wwcheckpoint();
	} else
		xxflush(1);
}

wwcheckpoint()
{
	int s = sigblock(sigmask(SIGALRM) | sigmask(SIGIO));

	tt.tt_ack = 0;
	do {
		(*tt.tt_checkpoint)();
#ifndef OLD_TTY
		(void) tcdrain(1);
#endif
		(void) alarm(3);
		for (wwdocheckpoint = 0; !wwdocheckpoint && tt.tt_ack == 0;)
			(void) sigpause(s);
	} while (tt.tt_ack == 0);
	(void) alarm(0);
	wwdocheckpoint = 0;
	if (tt.tt_ack < 0) {
		wwcopyscreen(wwcs, wwos);
		(void) alarm(1);
		wwreset();
		wwupdate();
		wwflush();
	} else {
		wwcopyscreen(wwos, wwcs);
		(void) alarm(3);
	}
	(void) sigsetmask(s);
}

wwcopyscreen(s1, s2)
	register union ww_char **s1, **s2;
{
	register i;
	register s = wwncol * sizeof **s1;

	for (i = wwnrow; --i >= 0;)
		bcopy((char *) *s1++, (char *) *s2++, s);
}

void
wwalarm()
{
	wwdocheckpoint = 1;
}
