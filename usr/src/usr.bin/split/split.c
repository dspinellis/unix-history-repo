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
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)split.c	4.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <stdio.h>
#include <ctype.h>

#define DEFLINE	1000			/* default num lines per file */
#define ERR	-1			/* general error */
#define ERREXIT	0			/* error exit */
#define NO	0			/* no/false */
#define OK	0			/* okay exit */
#define YES	1			/* yes/true */

static long	bytecnt,		/* byte count to split on */
		numlines;		/* lines in each file */
static int	ifd = ERR,		/* input file descriptor */
		ofd = ERR;		/* output file descriptor */
static short	file_open;		/* if a file open */
static char	bfr[MAXBSIZE],		/* I/O buffer */
		fname[MAXPATHLEN];	/* file name */

main(argc, argv)
	int	argc;
	char	**argv;
{
	register int	cnt;		/* general counter */
	long	atol();
	char	*strcpy();

	for (cnt = 1; cnt < argc; ++cnt) {
		if (argv[cnt][0] == '-')
			switch(argv[cnt][1]) {
			case 0:		/* stdin by request */
				if (ifd != ERR)
					usage();
				ifd = 0;
				break;
			case 'b':	/* byte count split */
				if (numlines)
					usage();
				if (!argv[cnt][2])
					bytecnt = atol(argv[++cnt]);
				else
					bytecnt = atol(argv[cnt] + 2);
				if (bytecnt <= 0) {
					fputs("split: byte count must be greater than zero.\n", stderr);
					usage();
				}
				break;
			default:
				if (!isdigit(argv[cnt][1]) || bytecnt)
					usage();
				if ((numlines = atol(argv[cnt] + 1)) <= 0) {
					fputs("split: line count must be greater than zero.\n", stderr);
					usage();
				}
				break;
			}
		else if (ifd == ERR) {		/* input file */
			if ((ifd = open(argv[cnt], O_RDONLY, 0)) < 0) {
				perror(argv[cnt]);
				exit(ERREXIT);
			}
		}
		else if (!*fname)		/* output file prefix */
			strcpy(fname, argv[cnt]);
		else
			usage();
	}
	if (ifd == ERR)				/* stdin by default */
		ifd = 0;
	if (bytecnt)
		split1();
	if (!numlines)
		numlines = DEFLINE;
	split2();
}

/*
 * split1 --
 *	split by bytes
 */
static
split1()
{
	register long	bcnt;		/* byte counter */
	register int	dist,		/* buffer offset */
			len;		/* read length */
	register char	*C;		/* tmp pointer into buffer */

	for (bcnt = 0;;)
		switch(len = read(ifd, bfr, MAXBSIZE)) {
		case 0:
			exit(OK);
		case ERR:
			perror("read");
			exit(ERREXIT);
		default:
			if (!file_open) {
				newfile();
				file_open = YES;
			}
			if (bcnt + len >= bytecnt) {
				dist = bytecnt - bcnt;
				write(ofd, bfr, dist);
				len -= dist;
				for (C = bfr + dist; len >= bytecnt; len -= bytecnt, C += bytecnt) {
					newfile();
					write(ofd, C, (int)bytecnt);
				}
				if (len) {
					newfile();
					write(ofd, C, len);
				}
				else
					file_open = NO;
				bcnt = len;
			}
			else {
				bcnt += len;
				write(ofd, bfr, len);
			}
		}
}

/*
 * split2 --
 *	split by lines
 */
static
split2()
{
	register char	*Ce,			/* start/end pointers */
			*Cs;
	register long	lcnt;			/* line counter */
	register int	len;			/* read length */

	for (lcnt = 0;;)
		switch(len = read(ifd, bfr, MAXBSIZE)) {
		case 0:
			exit(0);
		case ERR:
			perror("read");
			break;
		default:
			if (!file_open) {
				newfile();
				file_open = YES;
			}
			for (Cs = Ce = bfr; len--; Ce++)
				if (*Ce == '\n' && ++lcnt == numlines) {
					write(ofd, Cs, (int)(Ce - Cs) + 1);
					lcnt = 0;
					Cs = Ce + 1;
					if (len)
						newfile();
					else
						file_open = NO;
				}
			if (Cs < Ce)
				write(ofd, Cs, (int)(Ce - Cs));
		}
}

/*
 * newfile --
 *	open a new file
 */
static
newfile()
{
	static long	fnum;		/* file name counter */
	static short	defname;	/* using default name, "x" */
	static char	*fpnt;		/* output file name pointer */

	if (ofd == ERR) {
		if (fname[0]) {
			fpnt = fname + strlen(fname);
			defname = NO;
		}
		else {
			fname[0] = 'x';
			fpnt = fname + 1;
			defname = YES;
		}
		ofd = fileno(stdout);
	}
	/*
	 * hack to increase max files; original code just wandered through
	 * magic characters.  Maximum files is 3 * 26 * 26 == 2028
	 */
#define MAXFILES	676
	if (fnum == MAXFILES) {
		if (!defname || fname[0] == 'z') {
			fputs("split: too many files.\n", stderr);
			exit(ERREXIT);
		}
		++fname[0];
		fnum = 0;
	}
	fpnt[0] = fnum / 26 + 'a';
	fpnt[1] = fnum % 26 + 'a';
	++fnum;
	if (!freopen(fname, "w", stdout)) {
		fprintf(stderr, "split: unable to write to %s.\n", fname);
		exit(ERR);
	}
}

/*
 * usage --
 *	print usage message and die
 */
static
usage()
{
	fputs("usage: split [-] [-#] [-b byte_count] [file [prefix]]\n", stderr);
	exit(ERREXIT);
}
