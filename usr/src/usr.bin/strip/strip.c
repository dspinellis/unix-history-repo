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
static char sccsid[] = "@(#)strip.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <a.out.h>
#include <stdio.h>

typedef struct exec EXEC;

/* ARGSUSED */
main(argc, argv)
	int argc;
	char **argv;
{
	register off_t fsize;
	register int fd, n, pagesize;
	EXEC head;
	off_t lseek();

	pagesize = getpagesize();
	while (*++argv) {
		if ((fd = open(*argv, O_RDWR)) < 0 ||
		    (n = read(fd, (char *)&head, sizeof(EXEC))) == -1)
			error(*argv);
		if (n != sizeof(EXEC) || N_BADMAG(head)) {
			(void)fprintf(stderr,
			    "strip: %s not in a.out format.\n", *argv);
			exit(1);
		}
		if (!head.a_syms && !head.a_trsize && !head.a_drsize)
			continue;
		fsize = head.a_text + head.a_data;
		if (head.a_magic == ZMAGIC)
			fsize += pagesize - sizeof(EXEC);
		head.a_syms = head.a_trsize = head.a_drsize = 0;
		if (ftruncate(fd, fsize + sizeof(EXEC)) ||
		    lseek(fd, 0L, L_SET) == -1 ||
		    write(fd, (char *)&head, sizeof(EXEC)) != sizeof(EXEC))
			error(*argv);
		(void)close(fd);
	}
	exit(0);
}

error(fname)
	char *fname;
{
	extern int errno;
	char *strerror();

	(void)fprintf(stderr, "strip: %s: %s.\n", fname, strerror(errno));
	exit(1);
}
