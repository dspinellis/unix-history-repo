/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pt_tcp.c	8.2 (Berkeley) %G%
 *
 * $Id: pt_tcp.c,v 1.1 1992/05/25 21:43:09 jsp Exp jsp $
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/syslog.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#include "portald.h"

/*
 * Key will be tcp/host/port[/"priv"]
 * Create a TCP socket connected to the
 * requested host and port.
 * Some trailing suffix values have special meanings.
 * An unrecognised suffix is an error.
 */
int portal_tcp(pcr, key, v, kso, fdp)
struct portal_cred *pcr;
char *key;
char **v;
int kso;
int *fdp;
{
	char host[MAXHOSTNAMELEN];
	char port[MAXHOSTNAMELEN];
	char *p = key + (v[1] ? strlen(v[1]) : 0);
	char *q;
	struct hostent *hp;
	struct servent *sp;
	struct in_addr **ipp;
	struct in_addr *ip[2];
	struct in_addr ina;
	int s_port;
	int priv = 0;
	struct sockaddr_in sain;

	q = strchr(p, '/');
	if (q == 0 || q - p >= sizeof(host))
		return (EINVAL);
	*q = '\0';
	strcpy(host, p);
	p = q + 1;

	q = strchr(p, '/');
	if (q)
		*q = '\0';
	if (strlen(p) >= sizeof(port))
		return (EINVAL);
	strcpy(port, p);
	if (q) {
		p = q + 1;
		if (strcmp(p, "priv") == 0) {
			if (pcr->pcr_uid == 0)
				priv = 1;
			else
				return (EPERM);
		} else {
			return (EINVAL);
		}
	}

	hp = gethostbyname(host);
	if (hp != 0) {
		ipp = (struct in_addr **) hp->h_addr_list;
	} else {
		ina.s_addr = inet_addr(host);
		if (ina.s_addr == INADDR_NONE)
			return (EINVAL);
		ip[0] = &ina;
		ip[1] = 0;
		ipp = ip;
	}

	sp = getservbyname(port, "tcp");
	if (sp != 0)
		s_port = sp->s_port;
	else {
		s_port = atoi(port);
		if (s_port == 0)
			return (EINVAL);
	}

	bzero(&sain, sizeof(sain));
	sain.sin_len = sizeof(sain);
	sain.sin_family = AF_INET;
	sain.sin_port = s_port;

	while (ipp[0]) {
		int so;

		if (priv)
			so = rresvport((int *) 0);
		else
			so = socket(AF_INET, SOCK_STREAM, 0);
		if (so < 0) {
			syslog(LOG_ERR, "socket: %m");
			return (errno);
		}

		sain.sin_addr = *ipp[0];
		if (connect(so, (struct sockaddr *) &sain, sizeof(sain)) == 0) {
			*fdp = so;
			return (0);
		}
		(void) close(so);

		ipp++;
	}

	return (errno);
}
