#ifndef lint
static	char *sccsid = "@(#)wwiomux.c	3.9 84/03/06";
#endif

#include "ww.h"
#include <sys/time.h>

/*
 * Multiple window output handler.
 * The idea is to copy window outputs to the terminal, via the
 * display package.  We try to give the top most window highest
 * priority.  The only return condition is when there is keyboard
 * input, which is serviced asynchronously by wwrint().
 * When there's nothing to do, we sleep in a select().
 * This can be done better with interrupt driven io.  But that's
 * not supported on ptys, yet.
 * The history of this routine is interesting.
 */
wwiomux()
{
	register struct ww *w;
	int imask;
	register n;
	register char *p;
	char c;
	static struct timeval tv = { 0, 0 };
	char noblock;

loop:
	if (wwinterrupt())
		return;

	imask = 0;
	noblock = 0;
	for (w = wwhead.ww_forw; w != &wwhead; w = w->ww_forw) {
		if (w->ww_pty < 0)
			continue;
		imask |= 1 << w->ww_pty;
		if (w->ww_obc > 0 && !w->ww_stopped)
			noblock = 1;
	}

	if (!noblock) {
		if (wwcurwin != 0)
			wwcurtowin(wwcurwin);
		wwupdate();
		wwflush();
		if (setjmp(wwjmpbuf))
			return;
		wwsetjmp = 1;
		if (wwinterrupt()) {
			wwsetjmp = 0;
			return;
		}
	}
	wwnselect++;
	n = select(wwdtablesize, &imask, (int *)0, (int *)0,
		noblock ? &tv : (struct timeval *)0);
	wwsetjmp = 0;

	if (n < 0)
		wwnselecte++;
	else {
		if (n == 0)
			wwnselectz++;
		for (w = wwhead.ww_forw; w != &wwhead; w = w->ww_forw)
			if (w->ww_pty >= 0 && imask & 1 << w->ww_pty) {
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
					n--;
					wwnwreadd++;
					wwnwreadc += n;
					w->ww_obc += n;
				} else {
					wwnwreadp++;
					if (*p & TIOCPKT_STOP)
						w->ww_stopped = 1;
					if (*p & TIOCPKT_START)
						w->ww_stopped = 0;
					if (*p & TIOCPKT_FLUSHWRITE) {
						w->ww_stopped = 0;
						w->ww_obp = w->ww_ob;
						w->ww_obc = 0;
					}
				}
				*p = c;
			}
	}
	for (w = wwhead.ww_forw; w != &wwhead; w = w->ww_forw)
		if (w->ww_pty >= 0 && w->ww_obc != 0 && !w->ww_stopped) {
			n = wwwrite(w, w->ww_obp, w->ww_obc);
			if (w->ww_obc -= n)
				w->ww_obp += n;
			else
				w->ww_obp = w->ww_ob;
			if (wwinterrupt())
				return;
			break;
		}
	goto loop;
}
