/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific written prior permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* !lint */

#ifndef lint
static char sccsid[] = "@(#)cmp.c	4.8 (Berkeley) %G%";
#endif /* !lint */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#define DIFF	1			/* found differences */
#define ERR	2			/* error during run */
#define NO	0			/* no/false */
#define OK	0			/* didn't find differences */
#define YES	1			/* yes/true */

static int	fd1, fd2,		/* file descriptors */
		silent = NO;		/* if silent run */
static short	all = NO;		/* if report all differences */
static u_char	buf1[MAXBSIZE],		/* read buffers */
		buf2[MAXBSIZE];
static char	*file1, *file2;		/* file names */

main(argc, argv)
	int	argc;
	char	**argv;
{
	extern char	*optarg;
	extern int	optind;
	int	ch;
	u_long	otoi();

	while ((ch = getopt(argc, argv, "ls")) != EOF)
		switch(ch) {
		case 'l':		/* print all differences */
			all = YES;
			break;
		case 's':		/* silent run */
			silent = YES;
			break;
		case '?':
		default:
			usage();
		}
	argv += optind;
	argc -= optind;

	if (argc < 2 || argc > 4)
		usage();

	/* open up files; "-" is stdin */
	file1 = argv[0];
	if (strcmp(file1, "-") && (fd1 = open(file1, O_RDONLY, 0)) < 0)
		error(file1);
	file2 = argv[1];
	if ((fd2 = open(file2, O_RDONLY, 0)) < 0)
		error(file2);

	/* handle skip arguments */
	if (argc > 2) {
		skip(otoi(argv[2]), fd1, file1);
		if (argc == 4)
			skip(otoi(argv[3]), fd2, file2);
	}
	cmp();
	/*NOTREACHED*/
}

/*
 * skip --
 *	skip first part of file
 */
static
skip(dist, fd, fname)
	register u_long	dist;		/* length in bytes, to skip */
	register int	fd;		/* file descriptor */
	char	*fname;			/* file name for error */
{
	register int	rlen;		/* read length */
	register int	nread;

	for (; dist; dist -= rlen) {
		rlen = MIN(dist, sizeof(buf1));
		if ((nread = read(fd, buf1, rlen)) != rlen) {
			if (nread < 0)
				error(fname);
			else
				endoffile(fname);
		}
	}
}

static
cmp()
{
	register u_char	*C1, *C2;	/* traveling pointers */
	register int	cnt,		/* counter */
			len1, len2;	/* read lengths */
	register long	byte,		/* byte count */
			line;		/* line count */
	short	dfound = NO;		/* if difference found */

	for (byte = 0, line = 1;;) {
		switch(len1 = read(fd1, buf1, MAXBSIZE)) {
		case -1:
			error(file1);
		case 0:
			/*
			 * read of file 1 just failed, find out
			 * if there's anything left in file 2
			 */
			switch(read(fd2, buf2, 1)) {
				case -1:
					error(file2);
				case 0:
					exit(dfound ? DIFF : OK);
				default:
					endoffile(file1);
			}
		}
		/*
		 * file1 might be stdio, which means that a read of less than
		 * MAXBSIZE might not mean an EOF.  So, read whatever we read
		 * from file1 from file2.
		 */
		if ((len2 = read(fd2, buf2, len1)) == -1)
			error(file2);
		if (bcmp(buf1, buf2, len2)) {
			if (silent)
				exit(DIFF);
			if (all) {
				dfound = YES;
				for (C1 = buf1, C2 = buf2, cnt = len2; cnt--; ++C1, ++C2) {
					++byte;
					if (*C1 != *C2)
						printf("%6ld %3o %3o\n", byte, *C1, *C2);
				}
			}
			else for (C1 = buf1, C2 = buf2;; ++C1, ++C2) {
				++byte;
				if (*C1 != *C2) {
					printf("%s %s differ: char %ld, line %ld\n", file1, file2, byte, line);
					exit(DIFF);
				}
				if (*C1 == '\n')
					++line;
			}
		}
		else {
			byte += len2;
			/*
			 * here's the real performance problem, we've got to
			 * count the stupid lines, which means that -l is a
			 * *much* faster version, i.e., unless you really
			 * *want* to know the line number, run -s or -l.
			 */
			if (!silent && !all)
				for (C1 = buf1, cnt = len2; cnt--;)
					if (*C1++ == '\n')
						++line;
		}
		/*
		 * couldn't read as much from file2 as from file1; checked
		 * here because there might be a difference before we got
		 * to this point, which would have precedence.
		 */
		if (len2 < len1)
			endoffile(file2);
	}
}

/*
 * otoi --
 *	octal/decimal string to u_long
 */
static u_long
otoi(C)
	register char	*C;		/* argument string */
{
	register u_long	val;		/* return value */
	register int	base;		/* number base */

	base = (*C == '0') ? 8 : 10;
	for (val = 0; isdigit(*C); ++C)
		val = val * base + *C - '0';
	return(val);
}

/*
 * error --
 *	print I/O error message and die
 */
static
error(filename)
	char *filename;
{
	extern int errno;
	int sverrno;

	if (!silent) {
		sverrno = errno;
		(void)fprintf(stderr, "cmp: %s: ", filename);
		errno = sverrno;
		perror((char *)NULL);
	}
	exit(ERR);
}

/*
 * endoffile --
 *	print end-of-file message and exit indicating the files were different
 */
static
endoffile(filename)
	char *filename;
{
	/* 32V put this message on stdout, S5 does it on stderr. */
	if (!silent)
		(void)fprintf(stderr, "cmp: EOF on %s\n", filename);
	exit(DIFF);
}

/*
 * usage --
 *	print usage and die
 */
static
usage()
{
	fputs("usage: cmp [-ls] file1 file2 [skip1] [skip2]\n", stderr);
	exit(ERR);
}
