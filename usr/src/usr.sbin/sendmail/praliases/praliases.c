/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)praliases.c	6.2 (Berkeley) %G%";
#endif /* not lint */

#include <ndbm.h>
#include <sendmail.h>

int
main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	DBM *dbp;
	datum content, key;
	char *filename;
	int ch;

	filename = "/etc/aliases";
	while ((ch = getopt(argc, argv, "f:")) != EOF)
		switch((char)ch) {
		case 'f':
			filename = optarg;
			break;
		case '?':
		default:
			(void)fprintf(stderr, "usage: praliases [-f file]\n");
			exit(EX_USAGE);
		}
	argc -= optind;
	argv += optind;

	if ((dbp = dbm_open(filename, O_RDONLY, 0)) == NULL) {
		(void)fprintf(stderr,
		    "praliases: %s: %s\n", filename, strerror(errno));
		exit(EX_OSFILE);
	}
	if (!argc)
		for (key = dbm_nextkey(dbp);
		    key.dptr != NULL; key = dbm_nextkey(dbp)) {
			content = dbm_fetch(dbp, key);
			(void)printf("%s:%s\n", key.dptr, content.dptr);
		}
	else for (; *argv; ++argv) {
		key.dptr = *argv;
		key.dsize = strlen(*argv) + 1;
		content = dbm_fetch(dbp, key);
		if (!content.dptr)
			(void)printf("%s: No such key\n", key.dptr);
		else
			(void)printf("%s:%s\n", key.dptr, content.dptr);
	}
	exit(EX_OK);
}
