#ifndef lint
static	char *sccsid = "@(#)wwgets.c	1.4 83/07/27";
#endif

#include "defs.h"

char *ibufp = ibuf;

bread()
{
	register n;
	register char *p;
	int imask;

	while (ibufc == 0) {
		wwflush();
		imask = 1 << 0;
		while (wwforce(&imask) < 0)
			;
		if ((imask & 1 << 0) == 0)
			continue;
		if (ibufc == 0) {
			p = ibufp = ibuf;
			n = sizeof ibuf;
		} else {
			p = ibufp + ibufc;
			n = (ibuf + sizeof ibuf) - p;
		}
		if ((n = read(0, p, n)) > 0) {
			ibufc += n;
			nreadc += n;
		} else if (n == 0)
			nreadz++;
		else
			nreade++;
		nread++;
	}
}
