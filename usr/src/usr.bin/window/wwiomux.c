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
static char sccsid[] = "@(#)wwiomux.c	3.27 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include <sys/time.h>
#include <sys/types.h>
#if !defined(OLD_TTY) && !defined(TIOCPKT_DATA)
#include <sys/ioctl.h>
#endif
#include <fcntl.h>

extern int _wwdtablesize;

/*
 * Multiple window output handler.
 * The idea is to copy window outputs to the terminal, via the
 * display package.  We try to give wwcurwin highest priority.
 * The only return conditions are when there is keyboard input
 * and when a child process dies, which are serviced by signal
 * catchers (wwrint() and wwchild()).
 * When there's nothing to do, we sleep in a select().
 * This can be done better with interrupt driven io.  But that's
 * not supported on ptys, yet.
 * The history of this routine is interesting.
 */
wwiomux()
{
	register struct ww *w;
	register struct ww *w;
	fd_set imask;
	register n;
	register char *p;
	char c;
	struct timeval tv;
	char noblock = 0;

	for (w = wwhead; w; w = w->ww_next)
		if (w->ww_pty >= 0)
			*imask |= 1 << w->ww_pty;
	n = select(_wwdtablesize, imask,
		(int *)0, (int *)0, (struct timeval *)0);
	}
	}
}
