#ifndef lint
static char sccsid[] = "@(#)wwrint.c	3.4 4/24/85";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"
#include <fcntl.h>
#include <sys/signal.h>

/*
 * Tty input interrupt handler.
 * (1) Read input into buffer (wwib*).
 * (2) If the flag wwsetjmp is true, do longjmp(wwjmpbuf) for asyncronous
 *     actions, and to avoid race conditions, clear wwsetjmp.
 * Currently, the last is used to get out of the blocking
 * select() in wwiomux().
 * To avoid race conditions, we only modify wwibq in here, except
 * when the buffer is empty; and everywhere else, we only change wwibp.
 * It should be completely safe.
 */
wwrint()
{
	register n;

	if (wwibp == wwibq)
		wwibp = wwibq = wwib;
	wwnread++;
	(void) fcntl(0, F_SETFL, FNDELAY|wwnewtty.ww_fflags);
	n = read(0, wwibq, wwibe - wwibq);
	(void) fcntl(0, F_SETFL, wwnewtty.ww_fflags);
	if (n > 0) {
		wwibq += n;
		wwnreadc += n;
	} else if (n == 0)
		wwnreadz++;
	else
		wwnreade++;
	if (wwinterrupt() && wwsetjmp) {
		wwsetjmp = 0;
		(void) sigsetmask(sigblock(0) & ~sigmask(SIGIO));
		longjmp(wwjmpbuf, 1);
	}
}
