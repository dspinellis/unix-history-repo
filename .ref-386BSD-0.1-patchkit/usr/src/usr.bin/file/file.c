/*
 * file - find type of a file or files - main program.
 *
 * Copyright (c) Ian F. Darwin, 1987.
 * Written by Ian F. Darwin.
 *
 * This software is not subject to any license of the American Telephone
 * and Telegraph Company or of the Regents of the University of California.
 *
 * Permission is granted to anyone to use this software for any purpose on
 * any computer system, and to alter it and redistribute it freely, subject
 * to the following restrictions:
 *
 * 1. The author is not responsible for the consequences of use of this
 *    software, no matter how awful, even if they arise from flaws in it.
 *
 * 2. The origin of this software must not be misrepresented, either by
 *    explicit claim or by omission.  Since few users ever read sources,
 *    credits must appear in the documentation.
 *
 * 3. Altered versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.  Since few users
 *    ever read sources, credits must appear in the documentation.
 *
 * 4. This notice may not be removed or altered.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "file.h"

#define USAGE		"usage: %s [-c] [-f namefile] [-m magicfile] file...\n"

#ifndef	lint
static char *moduleid = 
	"@(#)$Header: file.c,v 1.14 87/11/12 13:11:06 ian Exp $";
#endif	/* lint */
extern char *ckfmsg;
int 	debug = 0, 	/* huh? */
	nbytes = 0,	/* number of bytes read from a datafile */
	nmagic = 0;	/* number of valid magic[]s */
FILE *efopen();
#ifdef MAGIC
char *magicfile = MAGIC;	/* where magic be found */
#else
char *magicfile = "/etc/magic";	/* where magic be found */
#endif
char *progname;
struct stat statbuf;
struct utimbuf {	/* for utime(2), belongs in a .h file */
	time_t actime;	/* access time */
	time_t modtime;	/* modification time */
};

/*
 * main - parse arguments and handle options
 */
main(argc, argv)
int argc;
char *argv[];
{
	int c;
	int check = 0, didsomefiles = 0, errflg = 0, ret = 0;
	extern int optind;
	extern char *optarg;

	progname = argv[0];

	while ((c = getopt(argc, argv, "cdf:m:")) != EOF)
		switch (c) {
		case 'c':
			++check;
			break;
		case 'd':
			++debug;
			break;
		case 'f':
			unwrap(optarg);
			++didsomefiles;
			break;
		case 'm':
			magicfile = optarg;
			break;
		case '?':
		default:
			errflg++;
			break;
		}
	if (errflg) {
		(void) fprintf(stderr, USAGE, progname);
		exit(2);
	}

	ret = apprentice(magicfile, check);
	if (check)
		exit(ret);

	if (optind == argc) {
		if (!didsomefiles)
			(void)fprintf(stderr, USAGE, progname);
	}
	else
		for (; optind < argc; optind++)
			process(argv[optind]);

	exit(0);
}

/*
 * unwrap -- read a file of filenames, do each one.
 */
unwrap(fn)
char *fn;
{
#define FILENAMELEN 128
	char buf[FILENAMELEN];
	FILE *f;

	if ((f = fopen(fn, "r")) == NULL)
		(void) fprintf(stderr, "%s: file %s unreadable\n",
			progname, fn);
	else {
		while (fgets(buf, FILENAMELEN, f) != NULL) {
			buf[strlen(buf)-1] = '\0';
			process(buf);
		}
		(void) fclose(f);
	}
}

/*
 * process - process input file
 */
process(inname)
char	*inname;
{
	int	fd;
	char	buf[HOWMANY];
	struct utimbuf utbuf;

	if (strcmp("-", inname) == 0) {
		(void) printf("standard input:\t");
		if (fstat(0, &statbuf)<0)
			warning("cannot fstat; ");
		fd = 0;
		goto readit;
	}
		
	(void) printf("%s:\t", inname);

	/*
	 * first try judging the file based on its filesystem status
	 * Side effect: fsmagic updates global data `statbuf'.
	 */
	if (fsmagic(inname) != 0) {
		/*NULLBODY*/;
	} else if ((fd = open(inname, 0)) < 0) {
		/* We can't open it, but we were able to stat it. */
		if (statbuf.st_mode & 0002) ckfputs("writeable, ", stdout);
		if (statbuf.st_mode & 0111) ckfputs("executable, ", stdout);
		warning("can't read");
	} else {
readit:
		/*
		 * try looking at the first HOWMANY bytes
		 */
		if ((nbytes = read(fd, buf, HOWMANY)) == -1)
			warning("read failed");
		if (nbytes == 0) {
			ckfputs("empty", stdout);
		} else
		/*
		 * try tests in /etc/magic (or surrogate magic file)
		 */
		if (softmagic(buf) == 1)
			/*NULLBODY*/;
		else if (ascmagic(buf) == 1)
			/*
			 * try known keywords, check for ascii-ness too.
			 */
			/*NULLBODY*/;
		else {
			/*
			 * abandon hope, all ye who remain here
			 */
			ckfputs("data", stdout);
		}
		if (strcmp("-", inname) != 0) {
			/*
			 * Restore access, modification times if we read it.
			 */
			utbuf.actime = statbuf.st_atime;
			utbuf.modtime = statbuf.st_mtime;
			(void) utime(inname, &utbuf);
			/* we don't care if we lack perms */
			(void) close(fd);
		}
	}

	(void) putchar('\n');
}


