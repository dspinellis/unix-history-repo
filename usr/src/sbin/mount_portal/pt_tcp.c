/*
 * Copyright (c) 1992 The Regents of the University of California
 * Copyright (c) 1990, 1992 Jan-Simon Pendry
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pt_tcp.c	1.2 (Berkeley) %G%
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

#include "portald.h"

/*
 * Key will be tcp/host/port[/"priv"]
 * Create a TCP socket connected to the
 * requested host and port.
 * Some trailing suffix values have special meanings.
 * An unrecognised suffix is an error.
 */
int portal_tcp(pcr, key, v, so, fdp)
struct portal_cred *pcr;
char *key;
char **v;
int so;
int *fdp;
{
	char host[MAXHOSTNAMELEN];
	char port[MAXHOSTNAMELEN];
	char *p = key + (v[1] ? strlen(v[1]) : 0);
	char *q;

	q = strchr(p, '/');
	if (q == 0 || q - p >= sizeof(host))
		return (EINVAL);
	*q = '\0';
	strcpy(host, p);
	p = q++;

	q = strchr(p, '/');
	if (q == 0 || q - p >= sizeof(port))
		return (EINVAL);
	*q = '\0';
	strcpy(port, p);
	p = q++;

	return (EHOSTUNREACH);
}
