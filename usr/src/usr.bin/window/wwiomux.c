#ifndef lint
static	char *sccsid = "@(#)wwiomux.c	3.7 84/01/16";
#endif

#include "ww.h"
#include <sys/time.h>

/*
 * Multiple window IO handler.
 */
wwiomux()
{
	register struct ww *w;
	int imask;
	char dont_block;
	register char *p;
	register n;
	char c;
	static struct timeval tv = { 0, 0 };

loop:
	imask = 1;
	dont_block = 0;
	for (w = wwhead.ww_forw; w != &wwhead; w = w->ww_forw) {
		if (w->ww_pty < 0)
			continue;
		if (w->ww_obp + w->ww_obc < w->ww_obe)
			imask |= 1 << w->ww_pty;
		if (w->ww_obc != 0 && !w->ww_stopped)
			dont_block = 1;
	}
	if (!dont_block) {
		wwupdate();
		wwflush();
	}
	wwnselect++;
	n = select(wwdtablesize, &imask, (int *)0, (int *)0,
		dont_block ? &tv : (struct timeval *)0);
	if (n < 0)
		wwnselecte++;
	else if (imask & 1) {
		if (wwibc == 0)
			p = wwibp = wwib;
		else
			p = wwibp + wwibc;
		n = wwibe - p;
		wwnread++;
		if ((n = read(0, p, n)) > 0) {
			wwibc += n;
			wwnreadc += n;
		} else if (n == 0)
			wwnreadz++;
		else
			wwnreade++;
	} else if (imask != 0 || dont_block) {
		char first_time = 1;
		if (n == 0)
			wwnselectz++;
		for (w = wwhead.ww_forw; w != &wwhead; w = w->ww_forw) {
			if (w->ww_pty < 0)
				continue;
			if (imask & 1 << w->ww_pty) {
				wwnwread++;
				p = w->ww_obp + w->ww_obc;
				if (p == w->ww_ob)
					w->ww_obp++;
				else
					p--;
				c = *p;
				n = read(w->ww_pty, p, w->ww_obe - p);
				if (n < 0) {
					wwnwreade++;
					(void) close(w->ww_pty);
					w->ww_pty = -1;
					continue;
				} else if (n == 0) {
					wwnwreadz++;
				} else if (*p == TIOCPKT_DATA) {
					wwnwreadd++;
					wwnwreadc += n - 1;
					w->ww_obc += n - 1;
				} else {
					wwnwreadp++;
					if (*p & TIOCPKT_STOP)
						w->ww_stopped = 1;
					if (*p & TIOCPKT_START)
						w->ww_stopped = 0;
					if (*p & TIOCPKT_FLUSHWRITE) {
						w->ww_obp = w->ww_ob;
						w->ww_obc = 0;
						w->ww_stopped = 0;
					}
				}
				*p = c;
			}
			if (first_time && w->ww_obc != 0 && !w->ww_stopped) {
				first_time = 0;
				/* XXX */
				n = wwwrite(w, w->ww_obp, MIN(w->ww_obc, 50));
				if (w->ww_obc -= n)
					w->ww_obp += n;
				else
					w->ww_obp = w->ww_ob;
			}
		}
	}
}
