/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)testdb.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <errno.h>
#include <limits.h>
#include <kvm.h>
#include <db.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <paths.h>

#include "extern.h"

/* Return true if the db file is valid, else false */
int
testdb()
{
	register DB *db;
	register int cc, kd, ret, dbversionlen;
	register char *cp, *uf;
	DBT rec;
	struct nlist nitem;
	char dbname[MAXPATHLEN], dbversion[_POSIX2_LINE_MAX];
	char kversion[_POSIX2_LINE_MAX];

	ret = 0;
	db = NULL;

	if ((kd = open(_PATH_KMEM, O_RDONLY, 0)) < 0)
		goto close;

	uf = _PATH_UNIX;
	if ((cp = rindex(uf, '/')) != 0)
		uf = cp + 1;
	(void) snprintf(dbname, sizeof(dbname), "%skvm_%s.db", _PATH_VARDB, uf);
	if ((db = dbopen(dbname, O_RDONLY, 0, DB_HASH, NULL)) == NULL)
		goto close;

	/* Read the version out of the database */
	rec.data = VRS_KEY;
	rec.size = sizeof(VRS_KEY) - 1;
	if ((db->get)(db, &rec, &rec, 0))
		goto close;
	if (rec.data == 0 || rec.size > sizeof(dbversion))
		goto close;
	bcopy(rec.data, dbversion, rec.size);
	dbversionlen = rec.size;

	/* Read version string from kernel memory */
	rec.data = VRS_SYM;
	rec.size = sizeof(VRS_SYM) - 1;
	if ((db->get)(db, &rec, &rec, 0))
		goto close;
	if (rec.data == 0 || rec.size != sizeof(struct nlist))
		goto close;
	bcopy(rec.data, &nitem, sizeof(nitem));
	/*
	 * Theoretically possible for lseek to be seeking to -1.  Not
	 * that it's something to lie awake nights about, however.
	 */
	errno = 0;
	if (lseek(kd, (off_t)nitem.n_value, SEEK_SET) == -1 && errno != 0)
		goto close;
	cc = read(kd, kversion, sizeof(kversion));
	if (cc < 0 || cc != sizeof(kversion))
		goto close;

	/* If they match, we win */
	ret = bcmp(dbversion, kversion, dbversionlen) == 0;

close:	if (kd >= 0)
		(void)close(kd);
	if (db)
		(void)(db->close)(db);
	return (ret);
}
