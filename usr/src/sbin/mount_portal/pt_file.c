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
 *	@(#)pt_file.c	8.1 (Berkeley) %G%
 *
 * $Id: pt_file.c,v 1.1 1992/05/25 21:43:09 jsp Exp jsp $
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/syslog.h>

#include "portald.h"

int portal_file(pcr, key, v, so, fdp)
struct portal_cred *pcr;
char *key;
char **v;
int so;
int *fdp;
{
	int fd;
	int gid;
	char pbuf[MAXPATHLEN];
	int error;
	int gidset[NGROUPS];
	int i;

	pbuf[0] = '/';
	strcpy(pbuf+1, key + (v[1] ? strlen(v[1]) : 0));

#ifdef DEBUG
	printf("path = %s, uid = %d, gid = %d\n", pbuf, pcr->pcr_uid, pcr->pcr_groups[0]);
#endif

	for (i = 0; i < pcr->pcr_ngroups; i++)
		gidset[i] = pcr->pcr_groups[i];

	if (setgroups(pcr->pcr_ngroups, gidset) < 0)
		return (errno);

	if (seteuid(pcr->pcr_uid) < 0)
		return (errno);

	fd = open(pbuf, O_RDWR|O_CREAT, 0666);
	if (fd < 0)
		error = errno;
	else
		error = 0;

	if (seteuid((uid_t) 0) < 0) {	/* XXX - should reset gidset too */
		error = errno;
		syslog(LOG_ERR, "setcred: %s", strerror(error));
		if (fd >= 0) {
			(void) close(fd);
			fd = -1;
		}
	}

	if (error == 0)
		*fdp = fd;

#ifdef DEBUG
	fprintf(stderr, "pt_file returns *fdp = %d, error = %d\n", *fdp, error);
#endif

	return (error);
}
