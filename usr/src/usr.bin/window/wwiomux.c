#ifndef lint
static char sccsid[] = "@(#)wwiomux.c	3.16 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"
#include <sys/time.h>
#include <sys/types.h>

/*
 * Multiple window output handler.
 * The idea is to copy window outputs to the terminal, via the
 * display package.  We try to give the top most window highest
 * priority.  The only return condition is when there is keyboard
 * input or when a child process dies which are serviced by signal
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
	static struct timeval tv = { 0, 0 };
	char noblock;

	for (;;) {
		if (wwinterrupt()) {
			wwclrintr();
			return;
		}

		FD_ZERO(&imask);
		noblock = 0;
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
		}
		wwnselect++;
		n = select(wwdtablesize, &imask, (fd_set *)0, (fd_set *)0,
			noblock ? &tv : (struct timeval *)0);
		wwsetjmp = 0;

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
		for (w = wwhead.ww_forw; w != &wwhead; w = w->ww_forw)
			if (w->ww_pty >= 0 && w->ww_obq > w->ww_obp &&
			    !w->ww_stopped) {
				n = wwwrite(w, w->ww_obp,
					w->ww_obq - w->ww_obp);
				if ((w->ww_obp += n) == w->ww_obq)
					w->ww_obq = w->ww_obp = w->ww_ob;
				if (wwinterrupt()) {
					wwclrintr();
					return;
				}
				break;
			}
	}
}
