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
static char sccsid[] = "@(#)wwiomux.c	3.21 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include <sys/time.h>
#include <sys/types.h>
#include <fcntl.h>

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
	fd_set imask;
	register n;
	register char *p;
	char c;
	struct timeval tv;
	char noblock = 0;

	for (;;) {
		if (wwinterrupt()) {
			wwclrintr();
			return;
		}

		FD_ZERO(&imask);
		for (w = wwhead.ww_forw; w != &wwhead; w = w->ww_forw) {
			if (w->ww_pty < 0)
				continue;
			if (w->ww_obq < w->ww_obe)
				FD_SET(w->ww_pty, &imask);
			if (w->ww_obq > w->ww_obp && !w->ww_stopped)
				noblock = 1;
		}

		if (!noblock) {
			if (wwcurwin != 0)
				wwcurtowin(wwcurwin);
			wwupdate();
			wwflush();
			setjmp(wwjmpbuf);
			wwsetjmp = 1;
			if (wwinterrupt()) {
				wwsetjmp = 0;
				wwclrintr();
				return;
			}
			/*
			 * Defensive code.  If somebody else (for example,
			 * wall) clears the ASYNC flag on us, we will block
			 * forever.  So we need a finite timeout and set
			 * the flag again.  Anything more clever will probably
			 * need even more system calls.  (This is a bug
			 * in the kernel.)
			 * I don't like this one bit.
			 */
			fcntl(0, F_SETFL, wwnewtty.ww_fflags);
			tv.tv_sec = 30;
			tv.tv_usec = 0;
		} else {
			tv.tv_sec = 0;
			tv.tv_usec = 10000;
		}
		wwnselect++;
		n = select(wwdtablesize, &imask, (fd_set *)0, (fd_set *)0, &tv);
		wwsetjmp = 0;
		noblock = 0;

		if (n < 0)
			wwnselecte++;
		else if (n == 0)
			wwnselectz++;
		else
			for (w = wwhead.ww_forw; w != &wwhead; w = w->ww_forw) {
				if (w->ww_pty < 0 ||
				    !FD_ISSET(w->ww_pty, &imask))
					continue;
				wwnwread++;
				p = w->ww_obq;
				if (w->ww_ispty) {
					if (p == w->ww_ob) {
						w->ww_obp++;
						w->ww_obq++;
					} else
						p--;
					c = *p;
				}
				n = read(w->ww_pty, p, w->ww_obe - p);
				if (n < 0) {
					wwnwreade++;
					(void) close(w->ww_pty);
					w->ww_pty = -1;
				} else if (n == 0) {
					wwnwreadz++;
					(void) close(w->ww_pty);
					w->ww_pty = -1;
				} else if (!w->ww_ispty) {
					wwnwreadd++;
					wwnwreadc += n;
					w->ww_obq += n;
				} else if (*p == TIOCPKT_DATA) {
					n--;
					wwnwreadd++;
					wwnwreadc += n;
					w->ww_obq += n;
				} else {
					wwnwreadp++;
					if (*p & TIOCPKT_STOP)
						w->ww_stopped = 1;
					if (*p & TIOCPKT_START)
						w->ww_stopped = 0;
					if (*p & TIOCPKT_FLUSHWRITE) {
						w->ww_stopped = 0;
						w->ww_obq = w->ww_obp =
							w->ww_ob;
					}
				}
				if (w->ww_ispty)
					*p = c;
			}
		/*
		 * Try the current window first, if there is output
		 * then process it and go back to the top to try again.
		 * This can lead to starvation of the other windows,
		 * but presumably that what we want.
		 * Update will eventually happen when output from wwcurwin
		 * dies down.
		 */
		if ((w = wwcurwin) != 0 && w->ww_pty >= 0 &&
		    w->ww_obq > w->ww_obp && !w->ww_stopped) {
			n = wwwrite(w, w->ww_obp, w->ww_obq - w->ww_obp);
			if ((w->ww_obp += n) == w->ww_obq)
				w->ww_obq = w->ww_obp = w->ww_ob;
			noblock = 1;
			continue;
		}
		for (w = wwhead.ww_forw; w != &wwhead; w = w->ww_forw)
			if (w->ww_pty >= 0 && w->ww_obq > w->ww_obp &&
			    !w->ww_stopped) {
				n = wwwrite(w, w->ww_obp,
					w->ww_obq - w->ww_obp);
				if ((w->ww_obp += n) == w->ww_obq)
					w->ww_obq = w->ww_obp = w->ww_ob;
				if (wwinterrupt())
					break;
			}
	}
}
