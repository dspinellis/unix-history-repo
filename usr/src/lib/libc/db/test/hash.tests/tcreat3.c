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
static char sccsid[] = "@(#)tcreat3.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <stdio.h>
#include <db.h>

#define INITIAL	25000
#define MAXWORDS    25000	       /* # of elements in search table */

char	wp1[8192];
char	wp2[8192];
main(argc, argv)
char **argv;
{
	DBT item, key;
	DB	*dbp;
	HASHINFO ctl;
	FILE *fp;
	int	trash;

	int i = 0;

	argv++;
	ctl.hash = NULL;
	ctl.bsize = atoi(*argv++);
	ctl.ffactor = atoi(*argv++);
	ctl.nelem = atoi(*argv++);
	ctl.lorder = 0;
	if (!(dbp = dbopen( "hashtest",
	    O_CREAT|O_TRUNC|O_RDWR, 0600, DB_HASH, &ctl))){
		/* create table */
		fprintf(stderr, "cannot create: hash table (size %d)\n",
			INITIAL);
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

/*
 * enter key/data pair into the table
 */
		if ((dbp->put)(dbp, &key, &item, R_NOOVERWRITE) != NULL) {
			fprintf(stderr, "cannot enter: key %s\n",
				item.data);
			exit(1);
		}			
	}

	(dbp->close)(dbp);
	exit(0);
}
