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
static char sccsid[] = "@(#)cat.c	5.4 (Berkeley) %G%";
#endif /* not lint */

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
	register FILE *fp;
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

	fp = stdin;
	filename = "-";
	do {
		if (*argv) {
			if (!strcmp(*argv, "-")) {
				fp = stdin;
				filename = "-";
			}
			else if (!(fp = fopen(filename = *argv, "r"))) {
				(void)fprintf(stderr, 
				    "cat: %s: %s\n", *argv, strerror(errno));
				++argv;
				continue;
			}
			++argv;
		}
		if (bflag || eflag || nflag || sflag || tflag || vflag)
			process_buf(fp);
		else
			rawcat(fileno(fp));
		if (fp != stdin)
			(void)fclose(fp);
	} while (*argv);
	exit(rval);
}

process_buf(fp)
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

rawcat(fd)
	register int fd;
{
	extern int errno;
	static char buf[8*1024];
	register int nr, nw, off;
	char *strerror();

	while ((nr = read(fd, buf, sizeof(buf))) > 0)
		for (off = 0; off < nr;) {
			if ((nw = write(fileno(stdout), buf + off, nr)) < 0) {
				perror("cat: stdout");
				rval = 1;
				return;
			}
			off += nw;
		}
	if (nr < 0) {
		(void)fprintf(stderr, "cat: %s: %s\n", filename,
		    strerror(errno));
		rval = 1;
	}
}
