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
static char sccsid[] = "@(#)tee.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <signal.h>
#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	register int cnt, n, step;
	int append, ch, *fd;
	char buf[8192], *malloc();
	off_t lseek();

	append = 0;
	while ((ch = getopt(argc, argv, "ai")) != EOF)
		switch((char)ch) {
		case 'a':
			append = 1;
			break;
		case 'i':
			(void)signal(SIGINT, SIG_IGN);
			break;
		case '?':
		default:
			fprintf(stderr, "usage: tee [-ai] [file ...]\n");
			exit(2);
		}
	argv += optind;
	argc -= optind;

	if (!(fd = (int *)malloc((u_int)((argc + 1) * sizeof(int))))) {
		fprintf(stderr, "tee: out of space.\n");
		exit(2);
	}
	fd[0] = 1;			/* always write to stdout */
	for (cnt = 1; *argv; ++argv)
		if ((fd[cnt] = open(*argv, append ? O_WRONLY|O_CREAT :
		    O_WRONLY|O_CREAT|O_TRUNC, 0600)) < 0) {
			fprintf(stderr, "tee: %s: ", *argv);
			perror((char *)NULL);
		}
		else {
			if (append)
				(void)lseek(fd[cnt], 0L, L_XTND);
			++cnt;
		}
	for (--cnt; (n = read(0, buf, sizeof(buf))) > 0;)
		for (step = cnt; step >= 0; --step)
			(void)write(fd[step], buf, n);
	exit(0);
}
