#ifndef lint
static	char *sccsid = "@(#)wwiomux.c	1.4 83/07/19";
#endif

#include "ww.h"

extern int _wwdtablesize;

wwforce(imask)
register int *imask;
{
	register struct ww *w;
	char buf[512];
	register int n;

	for (w = wwhead; w; w = w->ww_next)
		if (w->ww_pty >= 0)
			*imask |= 1 << w->ww_pty;
	n = select(_wwdtablesize, imask,
		(int *)0, (int *)0, (struct timeval *)0);
	if (n <= 0)
		return -1;
	for (w = wwhead; w; w = w->ww_next) {
		if (*imask & 1<<w->ww_pty) {
			n = read(w->ww_pty, buf, sizeof buf);
			if (n > 0)
				wwwrite(w, buf, n);
		}
	}
	return 0;
}
