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
static char sccsid[] = "@(#)size.c	4.6 (Berkeley) 6/18/88";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <a.out.h>
#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	struct exec head;
	u_long total;
	int exval, fd, first;

	if (!*argv[1])
		*argv = "a.out";
	else
		++argv;
	for (first = 1, exval = 0; *argv; ++argv) {
		if ((fd = open(*argv, O_RDONLY, 0)) < 0) {
			fprintf(stderr, "size: ");
			perror(*argv);
			exval = 1;
			continue;
		}
		if (read(fd, (char *)&head, sizeof(head)) != sizeof(head) ||
		    N_BADMAG(head)) {
			fprintf(stderr, "size: %s: not in a.out format.\n",
			    *argv);
			exval = 1;
			continue;
		}
		(void)close(fd);
		if (first) {
			first = 0;
			printf("text\tdata\tbss\tdec\thex\n");
		}
		total = head.a_text + head.a_data + head.a_bss;
		printf("%lu\t%lu\t%lu\t%lu\t%lx", head.a_text, head.a_data,
		    head.a_bss, total, total);
		if (argc > 2)
			printf("\t%s", *argv);
		printf("\n");
	}
	exit(exval);
}
