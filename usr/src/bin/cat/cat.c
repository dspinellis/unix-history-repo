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
static char sccsid[] = "@(#)cat.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>
#include <ctype.h>

char *filename;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno, optind;
	register int fd, ch, rval;
	char *strerror();

	while ((ch = getopt(argc, argv, "-u")) != EOF)
		switch (ch) {
		case '-':
			--optind;
			goto done;
		case 'u':			/* always unbuffered */
			break;
		case '?':
			(void)fprintf(stderr,
			    "usage: cat [-u] [-] [file ...]\n");
			exit(1);
		}
done:	argv += optind;

	fd = fileno(stdin);
	filename = "-";
	rval = 0;
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
		rval |= rawcat(fd);
		if (fd != fileno(stdin))
			(void)close(fd);
	} while (*argv);
	exit(rval);
}

rawcat(fd)
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
			fprintf(stderr, "cat: %s: no memory.\n", filename);
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
