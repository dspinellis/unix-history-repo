/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1991, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tread2.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <stdio.h>
#include <db.h>

#define INITIAL	25000
#define MAXWORDS    25000	       /* # of elements in search table */

typedef struct {		       /* info to be stored */
	int num, siz;
} info;

char	wp1[8192];
char	wp2[8192];
main(argc, argv)
char **argv;
{
	DBT item, key, res;
	DB	*dbp;
	HASHINFO ctl;
	int	stat;

	int i = 0;

	ctl.nelem = INITIAL;
	ctl.hash = NULL;
	ctl.bsize = 64;
	ctl.ffactor = 1;
	ctl.cachesize = atoi(*argv++);
	ctl.lorder = 0;
	if (!(dbp = dbopen( "hashtest", O_RDONLY, 0400, DB_HASH, &ctl))) {
		/* create table */
		fprintf(stderr, "cannot open: hash table\n" );
		exit(1);
	}

	key.data = wp1;
	item.data = wp2;
	while ( fgets(wp1, 8192, stdin) &&
		fgets(wp2, 8192, stdin) &&
		i++ < MAXWORDS) {
/*
* put info in structure, and structure in the item
*/
		key.size = strlen(wp1);
		item.size = strlen(wp2);

		stat = (dbp->get)(dbp, &key, &res,0);
		if (stat < 0) {
		    fprintf ( stderr, "Error retrieving %s\n", key.data );
		    exit(1);
		} else if ( stat > 0 ) {
		    fprintf ( stderr, "%s not found\n", key.data );
		    exit(1);
		}
	}
	(dbp->close)(dbp);
	exit(0);
}
