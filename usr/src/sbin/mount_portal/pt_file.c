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
 *	@(#)pt_file.c	1.1 (Berkeley) %G%
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

	pbuf[0] = '/';
	strcpy(pbuf+1, key + (v[1] ? strlen(v[1]) : 0));

#ifdef DEBUG
	printf("path = %s, uid = %d, gid = %d\n", pbuf, pcr->pcr_uid, pcr->pcr_gid);
#endif

	if (setregid(0, pcr->pcr_gid) < 0)
		return (errno);

	gid = pcr->pcr_gid;
	if (setgroups(1, &gid) < 0)
		return (errno);

	if (setreuid(0, pcr->pcr_uid) < 0)
		return (errno);

	fd = open(pbuf, O_RDWR|O_CREAT, 0666);
	if (fd < 0)
		error = errno;
	else
		error = 0;

	if (setreuid(0, 0) < 0 || setregid(0, 0) < 0) {
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
