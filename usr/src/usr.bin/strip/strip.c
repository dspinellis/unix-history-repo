/*
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
static char sccsid[] = "@(#)strip.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include <a.out.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void err __P((const char *fmt, ...));

typedef struct exec EXEC;

main(argc, argv)
	int argc;
	char *argv[];
{
	register off_t fsize;
	register int fd, n, pagesize;
	EXEC head;
	int ch, dstabs;

	dstabs = 0;
	while ((ch = getopt(argc, argv, "d")) != EOF)
		switch(ch) {
		case 'd':
			dstabs = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	pagesize = getpagesize();

	for (; *argv; ++argv) {
		if ((fd = open(*argv, O_RDWR)) < 0 ||
		    (n = read(fd, &head, sizeof(EXEC))) == -1) {
			err("%s: %s", *argv, strerror(errno));
			continue;
		}
		if (n != sizeof(EXEC) || N_BADMAG(head)) {
			err("%s: %s", *argv, strerror(EFTYPE));
			continue;
		}
		if (!head.a_syms && !head.a_trsize && !head.a_drsize) {
			(void)close(fd);
			continue;
		}
		fsize = head.a_text + head.a_data;
		if (head.a_magic == ZMAGIC)
			fsize += pagesize - sizeof(EXEC);
		head.a_syms = head.a_trsize = head.a_drsize = 0;

		if (ftruncate(fd, fsize + sizeof(EXEC)) ||
		    lseek(fd, 0L, SEEK_SET) == -1 ||
		    write(fd, &head, sizeof(EXEC)) != sizeof(EXEC))
			err("%s: %s", *argv, strerror(errno)); 
		(void)close(fd);
	}
	exit(0);
}

usage()
{
	(void)fprintf(stderr, "usage: strip [-d] file ...\n");
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
	(void)fprintf(stderr, "strip: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
}
