/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)bsdtcp.c	4.6 (Berkeley) %G%";
#endif /* not lint */

#include "condevs.h"
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>

/*
 *	bsdtcpopn -- make a tcp connection
 *
 *	return codes:
 *		>0 - file number - ok
 *		FAIL - failed
 */

bsdtcpopn(flds)
register char *flds[];
{
	struct servent *sp;
	struct hostent *hp;
	struct	sockaddr_in hisctladdr;
	int s = -1, port;
	extern int errno;
	extern char *sys_errlist[];

	sp = getservbyname(flds[F_CLASS], "tcp");
	if (sp == NULL) {
		port = htons(atoi(flds[F_CLASS]));
		if (port == 0) {
			logent(_FAILED, "UNKNOWN PORT NUMBER");
			return CF_SYSTEM;
		}
	} else
		port = sp->s_port;
	DEBUG(4, "bsdtcpopn host %s, ", flds[F_PHONE]);
	DEBUG(4, "port %d\n", ntohs(port));
	if (setjmp(Sjbuf)) {
		bsdtcpcls(s);
		logent("tcpopen", "TIMEOUT");
		return CF_DIAL;
	}

	bzero((char *)&hisctladdr, sizeof (hisctladdr));
	hp = gethostbyname(flds[F_PHONE]);
	if (hp == NULL) {
		logent("tcpopen","UNKNOWN HOST");
		return CF_DIAL;
	}
	signal(SIGALRM, alarmtr);
	alarm(MAXMSGTIME*2);
	hisctladdr.sin_family = hp->h_addrtype;
#ifdef BSD2_9
	s = socket(SOCK_STREAM, 0, &hisctladdr, 0);
#else BSD4_2
	s = socket(hp->h_addrtype, SOCK_STREAM, 0);
#endif BSD4_2
	if (s < 0)
		goto bad;
#ifndef BSD2_9
	if (bind(s, (struct sockaddr *)&hisctladdr, sizeof (hisctladdr)) < 0)
		goto bad;
#endif BSD2_9
	bcopy(hp->h_addr, (char *)&hisctladdr.sin_addr, hp->h_length);
	hisctladdr.sin_port = port;
#ifdef BSD2_9
	if (connect(s, (char *)&hisctladdr) < 0)
#else BSD4_2
	if (connect(s, (struct sockaddr *)&hisctladdr, sizeof (hisctladdr)) < 0)
#endif BSD4_2
		goto bad;
	alarm(0);
	CU_end = bsdtcpcls;
	return s;
bad:
	alarm(0);
	bsdtcpcls(s);
	DEBUG(5, "tcpopen failed: errno %d\n", errno);
	logent(sys_errlist[errno], _FAILED);
	return CF_DIAL;
}

/*
 * bsdtcpcls -- close tcp connection
 */
bsdtcpcls(fd)
register int fd;
{
	DEBUG(4, "TCP CLOSE called\n", 0);
	if (fd > 0) {
		close(fd);
		DEBUG(4, "closed fd %d\n", fd);
	}
}
