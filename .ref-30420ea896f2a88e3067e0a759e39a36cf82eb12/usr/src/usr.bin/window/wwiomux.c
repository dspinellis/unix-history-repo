#ifndef lint
static	char *sccsid = "@(#)wwiomux.c	3.3 83/08/26";
#endif

#include "ww.h"

wwforce(imask)
register int *imask;
{
	register struct ww **w;
	char buf[512];
	register int n;

	for (w = wwindex; w < &wwindex[NWW]; w++)
		if (*w && (*w)->ww_haspty && (*w)->ww_pty >= 0)
			*imask |= 1 << (*w)->ww_pty;
	if (*imask == 0)
		return -1;
	n = select(wwdtablesize, imask, (int *)0, (int *)0,
		(struct timeval *)0);
	if (n <= 0)
		return -1;
	for (w = wwindex; w < &wwindex[NWW]; w++)
		if (*w && (*w)->ww_haspty && (*w)->ww_pty >= 0
		    && *imask & 1 << (*w)->ww_pty) {
			n = read((*w)->ww_pty, buf, sizeof buf);
			if (n > 0)
				(void) wwwrite((*w), buf, n);
		}
	return 0;
}
