/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)devname.c	5.11 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <fcntl.h>
#include <ndbm.h>
#include <stdio.h>
#include <paths.h>

char *
devname(dev)
	dev_t dev;
{
	static DBM *db;
	static int failure;
	datum dp, key;

	if (!db && !failure && !(db = dbm_open(_PATH_DEVDB, O_RDONLY, 0))) {
		(void)fprintf(stderr,
		    "ps: no device database %s\n", _PATH_DEVDB);
		failure = 1;
	}
	if (failure)
		return("??");

	key.dptr = (char *)&dev;
	key.dsize = sizeof(dev);
	dp = dbm_fetch(db, key);
	return(dp.dptr ? dp.dptr : "??");
}
