/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)hsearch.c	8.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <fcntl.h>
#include <string.h>

#include <db.h>
#include "search.h"

static DB *dbp = NULL;
static ENTRY retval;

extern int
hcreate(nel)
	u_int nel;
{
	HASHINFO info;

	info.nelem = nel;
	info.bsize = 256;
	info.ffactor = 8;
	info.cachesize = NULL;
	info.hash = NULL;
	info.lorder = 0;
	dbp = (DB *)__hash_open(NULL, O_CREAT | O_RDWR, 0600, &info, 0);
	return ((int)dbp);
}

extern ENTRY *
hsearch(item, action)
	ENTRY item;
	ACTION action;
{
	DBT key, val;
	int status;

	if (!dbp)
		return (NULL);
	key.data = (u_char *)item.key;
	key.size = strlen(item.key) + 1;

	if (action == ENTER) {
		val.data = (u_char *)item.data;
		val.size = strlen(item.data) + 1;
		status = (dbp->put)(dbp, &key, &val, R_NOOVERWRITE);
		if (status)
			return (NULL);
	} else {
		/* FIND */
		status = (dbp->get)(dbp, &key, &val, 0);
		if (status)
			return (NULL);
		else
			item.data = (char *)val.data;
	}
	retval.key = item.key;
	retval.data = item.data;
	return (&retval);
}

extern void
hdestroy()
{
	if (dbp) {
		(void)(dbp->close)(dbp);
		dbp = NULL;
	}
}
