/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)dk.c	4.4 (Berkeley) %G%";
#endif /* not lint */

#include "condevs.h"
#ifdef DATAKIT
#include <dk.h>
#define DKTRIES 2
/***
 *	dkopn(flds)	make datakit connection
 *
 *	return codes:
 *		>0 - file number - ok
 *		FAIL - failed
 */
dkopn(flds)
char *flds[];
{
	int dkphone;
	register char *cp;
	register ret, i;

	if (setjmp(Sjbuf))
		return CF_DIAL;

	signal(SIGALRM, alarmtr);
	dkphone = 0;
	cp = flds[F_PHONE];
	while(*cp)
		dkphone = 10 * dkphone + (*cp++ - '0');
	DEBUG(4, "dkphone (%d) ", dkphone);
	for (i = 0; i < DKTRIES; i++) {
		getnextfd();
		ret = dkdial(D_SH, dkphone, 0);
		next_fd = -1;
		DEBUG(4, "dkdial (%d)\n", ret);
		if (ret > -1)
			break;
	}
	return ret;
}
#endif
