#ifndef lint
static	char *sccsid = "@(#)wwiomux.c	3.7 84/01/16";
#endif

#include "ww.h"
#include <sys/time.h>

extern int _wwdtablesize;

/*
 * Multiple window IO handler.
 */
wwiomux()
{
	register struct ww *w;
	register struct ww *w;
	int imask;
	char dont_block;
	register char *p;
	register n;
	char c;
	static struct timeval tv = { 0, 0 };

	for (w = wwhead; w; w = w->ww_next)
		if (w->ww_pty >= 0)
			*imask |= 1 << w->ww_pty;
	n = select(_wwdtablesize, imask,
		(int *)0, (int *)0, (struct timeval *)0);
		}
	}
	}
}
