/*-
 * Copyright (c) 1990, 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990, 1993 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)kvm_mkdb.c	5.18 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>

#include <db.h>
#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <paths.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "extern.h"

static void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	DB *db;
	int ch;
	char *p, *nlistpath, *nlistname, dbtemp[MAXPATHLEN], dbname[MAXPATHLEN];

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch (ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc > 1)
		usage();

	/* If the existing db file matches the currently running kernel, exit */
	if (testdb())
		exit(0);

#define	basename(cp)	((p = rindex((cp), '/')) != NULL ? p + 1 : (cp))
	nlistpath = argc > 0 ? argv[0] : _PATH_UNIX;
	nlistname = basename(nlistpath);

	(void)snprintf(dbtemp, sizeof(dbtemp), "%skvm_%s.tmp",
	    _PATH_VARDB, nlistname);
	(void)snprintf(dbname, sizeof(dbname), "%skvm_%s.db",
	    _PATH_VARDB, nlistname);
	(void)umask(0);
	db = dbopen(dbtemp, O_CREAT | O_EXLOCK | O_TRUNC | O_RDWR,
	    S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH, DB_HASH, NULL);
	if (db == NULL)
		err(1, "%s", dbtemp);
	create_knlist(nlistpath, db);
	if (db->close(db))
		err(1, "%s", dbtemp);
	if (rename(dbtemp, dbname))
		err(1, "rename %s to %s", dbtemp, dbname);
	exit(0);
}

void
usage()
{
	(void)fprintf(stderr, "usage: kvm_mkdb [file]\n");
	exit(1);
}
