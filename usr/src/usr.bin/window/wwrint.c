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
static char sccsid[] = "@(#)wwrint.c	3.7 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include <fcntl.h>
#include <sys/signal.h>

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
		wwsetintr();
	} else if (n == 0)
		wwnreadz++;
	else
		wwnreade++;
}
