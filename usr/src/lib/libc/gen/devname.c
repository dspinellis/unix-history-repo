/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)devname.c	5.13 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <fcntl.h>
#include <db.h>
#include <stdio.h>
#include <paths.h>

char *
devname(dev)
	dev_t dev;
{
	static DB *db;
	static int failure;
	DBT data, key;

	if (!db && !failure &&
	    !(db = hash_open(_PATH_DEVDB, O_RDONLY, 0, NULL))) {
		(void)fprintf(stderr,
		    "ps: no device database %s\n", _PATH_DEVDB);
		failure = 1;
	}
	if (failure)
		return("??");

	key.data = (u_char *)&dev;
	key.size = sizeof(dev);
	return((db->get)(db, &key, &data, 0L) ? "??" : (char *)data.data);
}
