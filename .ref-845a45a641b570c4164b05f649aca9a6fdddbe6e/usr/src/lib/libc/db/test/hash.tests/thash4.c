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
static char sccsid[] = "@(#)thash4.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <sys/timeb.h>
#include <stdio.h>
#include <errno.h>
#include <db.h>

#define INITIAL	25000
#define MAXWORDS    25000	       /* # of elements in search table */

/* Usage: thash pagesize fillfactor file */
char	wp1[8192];
char	wp2[8192];
main(argc, argv)
char **argv;
{
	DBT item, key, res;
	DB *dbp;
	HASHINFO ctl;
	FILE *fp;
	int	stat;
	time_t	t;

	int i = 0;

	argv++;
	ctl.hash = NULL;
	ctl.bsize = atoi(*argv++);
	ctl.ffactor = atoi(*argv++);
	ctl.nelem = atoi(*argv++);
	ctl.cachesize = atoi(*argv++);
	ctl.lorder = 0;
	if (!(dbp = dbopen( NULL, O_CREAT|O_RDWR, 0400, DB_HASH, &ctl))) {
		/* create table */
		fprintf(stderr, "cannot create: hash table size %d)\n",
			INITIAL);
		fprintf(stderr, "\terrno: %d\n", errno);
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
			fprintf(stderr, "\terrno: %d\n", errno);
			exit(1);
		}			
	}

	if ( --argc ) {
		fp = fopen ( argv[0], "r");
		i = 0;
		while ( fgets(wp1, 256, fp) && 
			fgets(wp2, 8192, fp) && 
			i++ < MAXWORDS) {

		    key.size = strlen(wp1);
		    stat = (dbp->get)(dbp, &key, &res, 0);
		    if (stat < 0 ) {
			fprintf ( stderr, "Error retrieving %s\n", key.data );
			fprintf(stderr, "\terrno: %d\n", errno);
			exit(1);
		    } else if ( stat > 0 ) {
			fprintf ( stderr, "%s not found\n", key.data );
			fprintf(stderr, "\terrno: %d\n", errno);
			exit(1);
		    }
		}
		fclose(fp);
	}
	dbp->close(dbp);
	exit(0);
}
