/*
 * Copyright (c) 1988 Regents of the University of California.
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
static char sccsid[] = "@(#)terminal.c	1.15 (Berkeley) %G%";
#endif /* not lint */

#include <arpa/telnet.h>
#include <sys/types.h>

#include "ring.h"

#include "externs.h"
#include "types.h"

Ring	ttyoring, ttyiring;
char	ttyobuf[2*BUFSIZ], ttyibuf[BUFSIZ];

int termdata;			/* Debugging flag */

char
    termEofChar,
    termEraseChar,
    termFlushChar,
    termIntChar,
    termKillChar,
#if	defined(MSDOS)
    termLiteralNextChar,
#endif	/* defined(MSDOS) */
    termQuitChar;

/*
 * initialize the terminal data structures.
 */

init_terminal()
{
    if (ring_init(&ttyoring, ttyobuf, sizeof ttyobuf) != 1) {
	exit(1);
    }
    if (ring_init(&ttyiring, ttyibuf, sizeof ttyibuf) != 1) {
	exit(1);
    }
    autoflush = TerminalAutoFlush();
}


/*
 *		Send as much data as possible to the terminal.
 *
 *		The return value indicates whether we did any
 *	useful work.
 */


int
ttyflush(drop)
int drop;
{
    register int n, n0, n1;

    n0 = ring_full_count(&ttyoring);
    if ((n1 = n = ring_full_consecutive(&ttyoring)) > 0) {
	if (drop) {
	    TerminalFlushOutput();
	    /* we leave 'n' alone! */
	} else {
	    n = TerminalWrite(ttyoring.consume, n);
	}
    }
    if (n > 0) {
	if (termdata && n) {
	    Dump('>', ttyoring.consume, n);
	}
	/*
	 * If we wrote everything, and the full count is
	 * larger than what we wrote, then write the
	 * rest of the buffer.
	 */
	if (n1 == n && n0 > n) {
		n1 = n0 - n;
		if (!drop)
			n1 = TerminalWrite(ttyoring.bottom, n1);
		n += n1;
	}
	ring_consumed(&ttyoring, n);
    }
    return n > 0;
}


/*
 * These routines decides on what the mode should be (based on the values
 * of various global variables).
 */


int
getconnmode()
{
    static char newmode[16] =
			{ 4, 5, 3, 3, 2, 2, 1, 1, 6, 6, 6, 6, 6, 6, 6, 6 };
    int modeindex = 0;

    if (dontlecho && (clocks.echotoggle > clocks.modenegotiated)) {
	modeindex += 1;
    }
    if (should_he(TELOPT_ECHO)) {
	modeindex += 2;
    }
    if (should_he(TELOPT_SGA)) {
	modeindex += 4;
    }
    if (In3270) {
	modeindex += 8;
    }
    return newmode[modeindex];
}

void
setconnmode()
{
    TerminalNewMode(getconnmode());
}


void
setcommandmode()
{
    TerminalNewMode(0);
}
