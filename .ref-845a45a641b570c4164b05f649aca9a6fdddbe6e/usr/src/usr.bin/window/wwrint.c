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
static char sccsid[] = "@(#)wwrint.c	3.14 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"
#if defined(OLD_TTY) || defined(VMIN_BUG)
#include <fcntl.h>
#endif

/*
 * Tty input interrupt handler.
 * (1) Read input into buffer (wwib*).
 * (2) Set the interrupt flag if anything is read.
 * Currently, the last is used to get out of the blocking
 * select() in wwiomux().
 * To avoid race conditions, we only modify wwibq in here, except
 * when the buffer is empty; and everywhere else, we only change wwibp.
 * It should be completely safe.
 */
void
wwrint()
{
	register n;

	if (wwibp == wwibq)
		wwibp = wwibq = wwib;
	wwnread++;
#if defined(OLD_TTY) || defined(VMIN_BUG)
	/* we have set c_cc[VMIN] to 0 */
	(void) fcntl(0, F_SETFL, O_NONBLOCK|wwnewtty.ww_fflags);
#endif
	n = read(0, wwibq, wwibe - wwibq);
#if defined(OLD_TTY) || defined(VMIN_BUG)
	(void) fcntl(0, F_SETFL, wwnewtty.ww_fflags);
#endif
	if (n > 0) {
		if (tt.tt_rint)
			n = (*tt.tt_rint)(wwibq, n);
		if (n > 0) {
			wwibq += n;
			wwnreadc += n;
			/*
			 * Hasten or delay the next checkpoint,
			 * as the case may be.
			 */
			if (tt.tt_checkpoint && !wwdocheckpoint)
				(void) alarm(1);
			wwsetintr();
		}
	} else if (n == 0)
		wwnreadz++;
	else
		wwnreade++;
}
