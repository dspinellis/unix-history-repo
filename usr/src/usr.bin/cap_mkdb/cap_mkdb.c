/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1992 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cap_mkdb.c	1.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <db.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static void	 db_build __P((char **));
static void	 err __P((const char *, ...));
static void	 getnamefield __P((char **, char *));
static void	 usage __P((void));

int docapdbunlink;
char *capdb, **inputfiles;

/*
 * Mkcapdb creates a capability hash database for quick retrieval of capability
 * records.  The database contains 2 types of entries: records and references
 * marked by the first byte in the data.  A record entry contains the actual
 * capability record whereas a reference contains the name (key) under which
 * the correct record is stored.
 */
int
main(argc, argv)
	int argc;
	char *argv[];
{
	int c;
	char *outname;

	outname = NULL;
	while ((c = getopt(argc, argv, "f:")) != EOF) {
		switch(c) {
		case 'f':
			outname = optarg;
			break;
		case '?':
		default:
			usage();
		}
	}		
	argc -= optind;
	argv += optind;	
	
	if (*argv == NULL)
		usage();

	inputfiles = argv;

	if (outname == NULL)
		outname = *inputfiles;

#define CAPDBNAMEEXTLEN		3	/* ".db" */
	if ((capdb = malloc(strlen(outname) + CAPDBNAMEEXTLEN + 1)) == NULL)
		err("%s", strerror(errno));
	(void)sprintf(capdb, "%s.db", outname);

        db_build(inputfiles);
	exit(0);
}
   
/*
 * Any changes to these definitions should be made also in the getcap(3)
 * library routines.
 */
#define	REFERENCE	(char)0
#define	RECORD		(char)1

#define NBUFSIZ		(8 * 1024)

/*
 * Db_build() builds the name and capabilty databases according to the
 * details above.
 */
void
db_build(inputfiles)
	char **inputfiles;
{
	DB *capdbp;
	DBT key, data;
	size_t lastlen, bplen;
	int st;
	char *cp, *np, *bp, *nf, namebuf[NBUFSIZ];

	if ((capdbp = dbopen(capdb, O_CREAT | O_TRUNC | O_WRONLY, 
            DEFFILEMODE, DB_HASH, NULL)) == NULL)
		err("%s: %s", capdb, strerror(errno));
	docapdbunlink = 1;
	
	lastlen = 0;
	nf = NULL;
	data.data = NULL;
	key.data = NULL;
	while((st = cgetnext(&bp, inputfiles)) > 0) {
		getnamefield(&nf, bp);
		if ((bplen = strlen(bp)) > lastlen) {
			if ((data.data =
			    realloc(data.data, bplen + 2)) == NULL)
				err("%s", strerror(errno));
			lastlen = bplen;
		}

		/*
		 * Store record under primary name.
		 */
		((char *)(data.data))[0] = RECORD;
		(void)strcpy(&((char *)(data.data))[1], bp);
		data.size = bplen + 2;
		key.data = namebuf;
		np = namebuf;
		cp = nf;
		for (;;) {
			if (*cp == ':' || *cp == '|') {
                                *np = '\0';
                                key.size = strlen(namebuf) + 1;
                                if (capdbp->put(capdbp, &key, &data, 0) < 0)
					err("put: %s", strerror(errno));
				cp++;
				break;
			}
			*np++ = *cp++;
		}

		/*
		 * Store references for other names.
		 */
		((char *)(data.data))[0] = REFERENCE;
		(void)strcpy(&((char *)(data.data))[1], namebuf);

		data.size = key.size + 1;	/* need extra byte for tag */

		for (np = namebuf; *cp != '\0'; cp++) {
			if (*cp == ':' || *cp == '|') {
				*np = '\0';
				key.size = strlen(namebuf) + 1;
				if (capdbp->put(capdbp, &key, &data, 0) < 0)
					err("put: %s", strerror(errno));
				np = namebuf;
				continue;
			}	      	      
			*np++ = *cp;
		}
	}
	if (capdbp->close(capdbp) < 0)
		err("%s", strerror(errno));
	if (st == -1)
		err("%s", strerror(errno));
	if (st == -2)
		err("potential reference loop detected");
	free(data.data);
	free(nf);
	free(bp);
}

void
getnamefield(nf, bp)
	char **nf, *bp;
{
	static size_t nfsize;
	size_t newsize;
	char *cp, tmp;
	
	for (cp = bp; *cp != ':'; cp++);

	tmp = *(cp + 1);
	*(cp + 1) = '\0';

	if ((newsize = cp - bp + 1) > nfsize) {
		if ((*nf = realloc(*nf, newsize)) == NULL)
			err("%s", strerror(errno));
		nfsize = newsize;
	}
	(void)strcpy(*nf, bp);
	*(cp + 1) = tmp;
}

void
usage()
{
        (void)fprintf(stderr,
	    "usage: cap_mkdb [-f outfile] file1 [file2 ...]\n");
        exit(1);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
	char *fmt;
        va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "cap_mkdb: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	if (docapdbunlink)
		(void)unlink(capdb);
	exit(1);
	/* NOTREACHED */
}
