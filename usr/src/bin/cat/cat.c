/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kevin Fall.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cat.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>
#include <ctype.h>

int bflag, eflag, nflag, sflag, tflag, vflag;
int rval;
char *filename;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno, optind;
	int ch;
	char *strerror();

	while ((ch = getopt(argc, argv, "-benstuv")) != EOF)
		switch (ch) {
		case '-':
			--optind;
			goto done;
		case 'b':
			bflag = nflag = 1;	/* -b implies -n */
			break;
		case 'e':
			eflag = vflag = 1;	/* -e implies -v */
			break;
		case 'n':
			nflag = 1;
			break;
		case 's':
			sflag = 1;
			break;
		case 't':
			tflag = vflag = 1;	/* -t implies -v */
			break;
		case 'u':
			setbuf(stdout, (char *)NULL);
			break;
		case 'v':
			vflag = 1;
			break;
		case '?':
			(void)fprintf(stderr,
			    "usage: cat [-benstuv] [-] [file ...]\n");
			exit(1);
		}
done:	argv += optind;

	if (bflag || eflag || nflag || sflag || tflag || vflag)
		cook_args(argv);
	else
		raw_args(argv);
	exit(rval);
}

cook_args(argv)
	char **argv;
{
	register FILE *fp;

	fp = stdin;
	filename = "-";
	do {
		if (*argv) {
			if (!strcmp(*argv, "-"))
				fp = stdin;
			else if (!(fp = fopen(*argv, "r"))) {
				(void)fprintf(stderr, 
				    "cat: %s: %s\n", *argv, strerror(errno));
				rval = 1;
				++argv;
				continue;
			}
			filename = *argv++;
		}
		cook_buf(fp);
		if (fp != stdin)
			(void)fclose(fp);
	} while (*argv);
}

cook_buf(fp)
	register FILE *fp;
{
	register int ch, gobble, line, prev;

	line = gobble = 0;
	for (prev = '\n'; (ch = getc(fp)) != EOF; prev = ch) {
		if (prev == '\n') {
			if (ch == '\n') {
				if (sflag) {
					if (gobble)
						continue;
					gobble = 1;
				}
				if (nflag && !bflag) {
					(void)fprintf(stdout, "%6d\t", ++line);
					if (ferror(stdout))
						break;
				}
			}
			else if (nflag) {
				(void)fprintf(stdout, "%6d\t", ++line);
				if (ferror(stdout))
					break;
			}
		}
		if (ch == '\n') {
			if (eflag)
				if (putc('$', stdout) == EOF)
					break;
		} else if (ch == '\t') {
			if (tflag) {
				if (putc('^', stdout) == EOF ||
				    putc('I', stdout) == EOF)
					break;
				continue;
			}
		} else if (vflag) {
			if (ch > 0177) {
				if (putc('M', stdout) == EOF ||
				    putc('-', stdout) == EOF)
					break;
				ch &= 0177;
			}
			if (iscntrl(ch)) {
				if (putc('^', stdout) == EOF ||
				    putc(ch == '\177' ? '?' :
				    ch | 0100, stdout) == EOF)
					break;
				continue;
			}
		}
		if (putc(ch, stdout) == EOF)
			break;
	}
	if (ferror(fp)) {
		(void)fprintf(stderr, "cat: %s: read error\n", filename);
		rval = 1;
	}
	if (ferror(stdout)) {
		clearerr(stdout);
		(void)fprintf(stderr, "cat: stdout: write error\n");
		rval = 1;
	}
}

raw_args(argv)
	char **argv;
{
	register int fd;

	fd = fileno(stdin);
	filename = "-";
	do {
		if (*argv) {
			if (!strcmp(*argv, "-"))
				fd = fileno(stdin);
			else if ((fd = open(*argv, O_RDONLY, 0)) < 0) {
				(void)fprintf(stderr, "cat: %s: %s\n",
				    *argv, strerror(errno));
				rval = 1;
				++argv;
				continue;
			}
			filename = *argv++;
		}
		rval |= raw_cat(fd);
		if (fd != fileno(stdin))
			(void)close(fd);
	} while (*argv);
}

raw_cat(fd)
	register int fd;
{
	extern int errno;
	register int nr, nw, off;
	static int bsize;
	static char *buf;
	struct stat sbuf;
	char *malloc(), *strerror();

	if (!buf) {
		if (fstat(fileno(stdout), &sbuf)) {
			(void)fprintf(stderr, "cat: %s: %s\n", filename,
			    strerror(errno));
			return(1);
		}
		bsize = MAX(sbuf.st_blksize, 1024);
		if (!(buf = malloc((u_int)bsize))) {
			(void)fprintf(stderr, "cat: %s: no memory.\n",
			    filename);
			return(1);
		}
	}
	while ((nr = read(fd, buf, bsize)) > 0)
		for (off = 0; off < nr;) {
			if ((nw = write(fileno(stdout), buf + off, nr)) < 0) {
				perror("cat: stdout");
				return(1);
			}
			off += nw;
		}
	if (nr < 0) {
		(void)fprintf(stderr, "cat: %s: %s\n", filename,
		    strerror(errno));
		return(1);
	}
	return(0);
}
