/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kevin Fall.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cat.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>

#include <ctype.h>
#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int bflag, eflag, nflag, sflag, tflag, vflag;
int rval;
char *filename;

void cook_args __P((char *argv[]));
void cook_buf __P((FILE *));
void raw_args __P((char *argv[]));
void raw_cat __P((int));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern int optind;
	int ch;

	while ((ch = getopt(argc, argv, "benstuv")) != EOF)
		switch (ch) {
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
	argv += optind;

	if (bflag || eflag || nflag || sflag || tflag || vflag)
		cook_args(argv);
	else
		raw_args(argv);
	if (fclose(stdout))
		err(1, "stdout");
	exit(rval);
}

void
cook_args(argv)
	char **argv;
{
	register FILE *fp;

	fp = stdin;
	filename = "stdin";
	do {
		if (*argv) {
			if (!strcmp(*argv, "-"))
				fp = stdin;
			else if ((fp = fopen(*argv, "r")) == NULL) {
				warn("%s", *argv);
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

void
cook_buf(fp)
	register FILE *fp;
{
	register int ch, gobble, line, prev;

	line = gobble = 0;
	for (prev = '\n'; (ch = getc(fp)) != EOF; prev = ch) {
		if (prev == '\n') {
			if (ch == '\n') {
				if (sflag) {
					if (!gobble && putchar(ch) == EOF)
						break;
					gobble = 1;
					continue;
				}
				if (nflag && !bflag) {
					(void)fprintf(stdout, "%6d\t", ++line);
					if (ferror(stdout))
						break;
				}
			} else if (nflag) {
				(void)fprintf(stdout, "%6d\t", ++line);
				if (ferror(stdout))
					break;
			}
		}
		gobble = 0;
		if (ch == '\n') {
			if (eflag)
				if (putchar('$') == EOF)
					break;
		} else if (ch == '\t') {
			if (tflag) {
				if (putchar('^') == EOF || putchar('I') == EOF)
					break;
				continue;
			}
		} else if (vflag) {
			if (!isascii(ch)) {
				if (putchar('M') == EOF || putchar('-') == EOF)
					break;
				ch = toascii(ch);
			}
			if (iscntrl(ch)) {
				if (putchar('^') == EOF ||
				    putchar(ch == '\177' ? '?' :
				    ch | 0100) == EOF)
					break;
				continue;
			}
		}
		if (putchar(ch) == EOF)
			break;
	}
	if (ferror(fp)) {
		warn("%s", filename);
		clearerr(fp);
	}
	if (ferror(stdout))
		err(1, "stdout");
}

void
raw_args(argv)
	char **argv;
{
	register int fd;

	fd = fileno(stdin);
	filename = "stdin";
	do {
		if (*argv) {
			if (!strcmp(*argv, "-"))
				fd = fileno(stdin);
			else if ((fd = open(*argv, O_RDONLY, 0)) < 0) {
				warn("%s", *argv);
				++argv;
				continue;
			}
			filename = *argv++;
		}
		raw_cat(fd);
		if (fd != fileno(stdin))
			(void)close(fd);
	} while (*argv);
}

void
raw_cat(rfd)
	register int rfd;
{
	register int nr, nw, off, wfd;
	static int bsize;
	static char *buf;
	struct stat sbuf;

	wfd = fileno(stdout);
	if (buf == NULL) {
		if (fstat(wfd, &sbuf))
			err(1, "%s", filename);
		bsize = MAX(sbuf.st_blksize, 1024);
		if ((buf = malloc((u_int)bsize)) == NULL)
			err(1, "");
	}
	while ((nr = read(rfd, buf, bsize)) > 0)
		for (off = 0; off < nr; nr -= nw, off += nw)
			if ((nw = write(wfd, buf + off, nr)) < 0)
				err(1, "stdout");
	if (nr < 0)
		warn("%s", filename);
}
