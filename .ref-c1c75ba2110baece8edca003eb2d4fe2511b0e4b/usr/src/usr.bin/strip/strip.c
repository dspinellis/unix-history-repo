/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)strip.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <a.out.h>
#include <stdio.h>

/* ARGSUSED */
main(argc, argv)
	int argc;
	char **argv;
{
	typedef struct exec EXEC;
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
			fprintf(stderr, "strip: %s not in a.out format.\n",
			    *argv);
			exit(1);
		}
		if (!head.a_syms && !head.a_trsize && !head.a_drsize) {
			fprintf(stderr, "strip: %s already stripped.\n", *argv);
			continue;
		}
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

static
error(fname)
	char *fname;
{
	fprintf(stderr, "strip: %s: ", fname);
	perror((char *)NULL);
	exit(1);
}
