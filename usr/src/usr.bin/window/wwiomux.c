#ifndef lint
static	char *sccsid = "@(#)wwiomux.c	1.1 83/07/12";
#endif

#include "ww.h"

wwforce(imask)
register int *imask;
{
	register struct ww *w;
	char buf[512];
	register int n;
	register char *p;

	for (w = _wwhead; w; w = w->ww_next)
		*imask |= 1 << w->ww_pty;
	n = select(getdtablesize(), imask,
		(int *)0, (int *)0, (struct timeval *)0);
	if (n <= 0)
		return -1;
	for (w = _wwhead; w; w = w->ww_next) {
		if (*imask & 1<<w->ww_pty) {
			n = read(w->ww_pty, buf, sizeof buf);
			for (p = buf; n-- > 0; p++)
				wwputc(*p, w);
		}
	}
	return 0;
}
