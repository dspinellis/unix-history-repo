/*
 * Copyright (c) 1983 Regents of the University of California.
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
static char sccsid[] = "@(#)uuencode.c	5.8 (Berkeley) %G%";
#endif /* not lint */

/*
 * uuencode [input] output
 *
 * Encode a file so it can be mailed to a remote system.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	extern int errno;
	struct stat sb;
	int mode;
	char *strerror();

	while (getopt(argc, argv, "") != EOF)
		usage();
	argv += optind;
	argc -= optind;

	switch(argc) {
	case 2:			/* optional first argument is input file */
		if (!freopen(*argv, "r", stdin) || fstat(fileno(stdin), &sb)) {
			(void)fprintf(stderr, "uuencode: %s: %s.\n",
			    *argv, strerror(errno));
			exit(1);
		}
#define	RWX	(S_IRWXU|S_IRWXG|S_IRWXO)
		mode = sb.st_mode & RWX;
		++argv;
		break;
	case 1:
#define	RW	(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH)
		mode = RW & ~umask(RW);
		break;
	case 0:
	default:
		usage();
	}

	(void)printf("begin %o %s\n", mode, *argv);
	encode();
	(void)printf("end\n");
	if (ferror(stdout)) {
		(void)fprintf(stderr, "uuencode: write error.\n");
		exit(1);
	}
	exit(0);
}

/* ENC is the basic 1 character encoding function to make a char printing */
#define	ENC(c) ((c) ? ((c) & 077) + ' ': '`')

/*
 * copy from in to out, encoding as you go along.
 */
encode()
{
	register int ch, n;
	register char *p;
	char buf[80];

	while (n = fread(buf, 1, 45, stdin)) {
		ch = ENC(n);
		if (putchar(ch) == EOF)
			break;
		for (p = buf; n > 0; n -= 3, p += 3) {
			ch = *p >> 2;
			ch = ENC(ch);
			if (putchar(ch) == EOF)
				break;
			ch = (*p << 4) & 060 | (p[1] >> 4) & 017;
			ch = ENC(ch);
			if (putchar(ch) == EOF)
				break;
			ch = (p[1] << 2) & 074 | (p[2] >> 6) & 03;
			ch = ENC(ch);
			if (putchar(ch) == EOF)
				break;
			ch = p[2] & 077;
			ch = ENC(ch);
			if (putchar(ch) == EOF)
				break;
		}
		if (putchar('\n') == EOF)
			break;
	}
	if (ferror(stdin)) {
		(void)fprintf(stderr, "uuencode: read error.\n");
		exit(1);
	}
	ch = ENC('\0');
	(void)putchar(ch);
	(void)putchar('\n');
}

usage()
{
	(void)fprintf(stderr,"usage: uuencode [infile] remotefile\n");
	exit(1);
}
