#ifndef lint
static	char *sccsid = "@(#)wwgets.c	1.1 83/07/18";
#endif

#include "defs.h"

char *ibufp = ibuf;

bread()
{
	register n;
	int imask;

	imask = 1 << 0;
	wwforce(&imask);
	if ((imask & 1<<0) == 0)
		return;
	if (ibufc == 0)
		ibufp = ibuf;
	n = read(0, ibufp + ibufc, ibuf + sizeof ibuf - ibufp - ibufc);
	if (n > 0)
		ibufc += n;
}
