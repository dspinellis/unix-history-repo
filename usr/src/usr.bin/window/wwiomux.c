#ifndef lint
static	char *sccsid = "@(#)wwiomux.c	3.8 84/03/03";
#endif

#include "ww.h"
#include <sys/time.h>

extern int _wwdtablesize;

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
	register struct ww *w;
	int imask;
	register n;
	register char *p;
	char c;
	static struct timeval tv = { 0, 0 };
	char noblock;

	for (w = wwhead; w; w = w->ww_next)
		if (w->ww_pty >= 0)
			*imask |= 1 << w->ww_pty;
	n = select(_wwdtablesize, imask,
		(int *)0, (int *)0, (struct timeval *)0);
		}
	}
	}
	goto loop;
}
