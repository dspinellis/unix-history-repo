#ifndef lint
static	char *sccsid = "@(#)wwiomux.c	3.5 83/12/01";
#endif

#include "ww.h"

wwforce(imask)
register int *imask;
{
	register struct ww **w;
	char buf[512];
	register int n;

	for (w = wwindex; w < &wwindex[NWW]; w++)
		if (*w && (*w)->ww_pty >= 0)
			*imask |= 1 << (*w)->ww_pty;
	if (*imask == 0)
		return -1;
	fflush(stdout);
	n = select(wwdtablesize, imask, (int *)0, (int *)0,
		(struct timeval *)0);
	if (n <= 0)
		return -1;
	for (w = wwindex; w < &wwindex[NWW]; w++)
		if (*w && (*w)->ww_pty >= 0 && *imask & 1 << (*w)->ww_pty) {
			n = read((*w)->ww_pty, buf, sizeof buf);
			if (n < 0) {
				(void) close((*w)->ww_pty);
				(*w)->ww_pty = -1;
			} else if (n > 0)
				(void) wwwrite(*w, buf, n);
		}
	return 0;
}
