/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1992 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cap_mkdb.c	1.3 (Berkeley) 8/11/92";
#endif /* not lint */

#include <sys/types.h>
#include <limits.h>
#include <db.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

static void	 db_build __P((char **));
static void	 error __P((const char *, ...));
static void	 getnamefield __P((char **, char *));
static void	 usage __P((void));

extern char *optarg;
extern int optind;

#define CAPDBNAMEEXTLEN	3	/* "*.db" */
#define BUFSIZ		1024

static char  *capdb;
static char **inputfiles;

/*
 * Mkcapdb creates a capability hash database for faster retrieval of 
 * capbaility records.  The database contains 2 types of entries: records and
 * references marked by the first byte in the data.  A record entry contains
 * the actual capability record whereas a reference contains the name (key)
 * under which the correct record is stored.
 */
void
main(argc, argv)
	int argc;
	char **argv;
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
		defualt:
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

	capdb = malloc(strlen(outname) + CAPDBNAMEEXTLEN + 1);
	if (capdb == NULL) {
		error("%s", strerror(ENOMEM));
		exit(1);
	}
	sprintf(capdb, "%s.db", outname);

        db_build(inputfiles);
	free(capdb);
	exit(0);
}
   
/*
 * Any changes to these definitions should be made also in the getcap(3)
 * library routines.
 */

#define REFERENCE	(char)0
#define RECORD		(char)1

/*
 * Db_build() builds the name and capabilty databases according to the
 * details above.
 */

static void
db_build(inputfiles)
	char **inputfiles;
{
	DB *capdbp;
	DBT key, data;
	char *cp, *np, *bp, *nf, capstart;
	char namebuf[BUFSIZ];
	size_t lastlen, bplen;
	int st;

	if ((capdbp = dbopen(capdb, O_CREAT|O_TRUNC|O_WRONLY, 
            0644, DB_HASH, NULL)) == NULL) {
		error("Could not create capability database: %s\n", capdb);
		exit(1);
	}	
	
	lastlen = 0;
	while((st = cgetnext(&bp, inputfiles)) > 0) {
		getnamefield(&nf, bp);
		if ((bplen = strlen(bp)) > lastlen) {
			data.data = realloc(data.data, lastlen + 2);
			if (data.data == NULL) {
				error("%s", strerror(ENOMEM));
				unlink(capdb);
				exit(1);
			}
			lastlen = bplen;
		}

		/*
		 * Store record under primary name.
		 */
		((char *)(data.data))[0] = RECORD;
		strcpy(&((char *)(data.data))[1], bp);
		data.size = bplen + 2;
		key.data = namebuf;
		np = namebuf;
		cp = nf;
		for (;;) {
			if (*cp == ':' || *cp == '|') {
                                *np = '\0';
                                key.size = strlen(namebuf) + 1;
                                if (capdbp->put(capdbp, &key, &data, 0) < 0) {
					error("%s", strerror(errno));
                                        exit(1);
                                }
				cp++;
				break;
			}
			*np++ = *cp++;
		}

		/*
		 * Store references for other names.
		 */
		((char *)(data.data))[0] = REFERENCE;
		strcpy(&((char *)(data.data))[1], namebuf);

		data.size = key.size + 1;	/* need extra byte for tag */

		for (np = namebuf; *cp != '\0'; cp++) {
			if (*cp == ':' || *cp == '|') {
				*np = '\0';
				key.size = strlen(namebuf) + 1;
				if (capdbp->put(capdbp, &key, &data, 0) < 0) {
					error("%s", strerror(errno));
					exit(1);
				}
				np = namebuf;
				continue;
			}	      	      
			*np++ = *cp;
		}
	}
	if (st == -1) {
		error("%s", strerror(errno));
		unlink(capdb);
		exit(1);
	}		
	if (st == -2) {
		error("Potential reference loop detected\n");
		unlink(capdb);
		exit(1);
	}		
	if (capdbp->close(capdbp) < 0) {
		error("%s", strerror(errno));
		exit(1);
	}
	free(data.data);
	free(nf);
	free(bp);
}

static void
getnamefield(nf, bp)
	char **nf, *bp;
{
	static size_t nfsize = 0;
	size_t newsize;
	char *cp, tmp;
	
	for (cp = bp; *cp != ':'; cp++) 
		;

	tmp = *(cp+1);
	*(cp+1) = '\0';

	if ((newsize = cp - bp + 1) > nfsize) {
		*nf = realloc (*nf, newsize);
		if (*nf == NULL) {
			error("%s", strerror(ENOMEM));
			unlink(capdb);
			exit(1);
		}
		nfsize = newsize;
	}
	
	strcpy(*nf, bp);
	*(cp+1) = tmp;
}

static void
usage()
{
        (void)fprintf(stderr, "usage: mkcapdb [-f outfile] file1 [file2 ...]\n");
        exit(1);
}


#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

static void
#if __STDC__
error(const char *fmt, ...)
#else
error(fatal, fmt, va_alist)
        int fatal;
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
}
