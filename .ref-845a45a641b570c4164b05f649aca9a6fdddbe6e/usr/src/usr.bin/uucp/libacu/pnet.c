/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)pnet.c	4.5 (Berkeley) %G%";
#endif /* not lint */

#include "condevs.h"
#ifdef PNET

/***
 *	pnetopn(flds)
 *
 *	call remote machine via Purdue network
 *	use dial string as host name, speed as socket number
 *	- Steve Bellovin
 */
pnetopn(flds)
char *flds[];
{
	int fd;
	int socket;
	register char *cp;

	fd = pnetfile();
	DEBUG(4, "pnet fd - %d\n", fd);
	if (fd < 0) {
		logent("AVAILABLE DEVICE", "NO");
		return CF_NODEV;
	}
	socket = 0;
	for (cp = flds[F_CLASS]; *cp; cp++)
		socket = 10*socket + (*cp - '0');
	DEBUG(4, "socket - %d\n", socket);
	if (setjmp(Sjbuf)) {
		DEBUG(4, "pnet timeout  - %s\n", flds[F_PHONE]);
		return CF_DIAL;
	}
	signal(SIGALRM, alarmtr);
	DEBUG(4, "host - %s\n", flds[F_PHONE]);
	alarm(15);
	if (pnetscon(fd, flds[F_PHONE], socket) < 0) {
		DEBUG(4, "pnet connect failed - %s\n", flds[F_PHONE]);
		alarm(0);
		return CF_DIAL;
	}
	alarm(0);
	return fd;
}
#endif	PNET
