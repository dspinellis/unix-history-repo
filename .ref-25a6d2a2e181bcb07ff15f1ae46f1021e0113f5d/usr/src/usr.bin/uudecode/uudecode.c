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
static char sccsid[] = "@(#)uudecode.c	5.6 (Berkeley) %G%";
#endif /* not lint */

/*
 * uudecode [input]
 *
 * create the specified file, decoding as you go.
 * used with uuencode.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <pwd.h>

/* single character decode */
#define DEC(c)	(((c) - ' ') & 077)

main(argc, argv)
char **argv;
{
	FILE *in, *out;
	int mode;
	char dest[128];
	char buf[80];

	/* optional input arg */
	if (argc > 1) {
		if ((in = fopen(argv[1], "r")) == NULL) {
			perror(argv[1]);
			exit(1);
		}
		argv++; argc--;
	} else
		in = stdin;

	if (argc != 1) {
		printf("Usage: uudecode [infile]\n");
		exit(2);
	}

	/* search for header line */
	for (;;) {
		if (fgets(buf, sizeof buf, in) == NULL) {
			fprintf(stderr, "No begin line\n");
			exit(3);
		}
		if (strncmp(buf, "begin ", 6) == 0)
			break;
	}
	(void)sscanf(buf, "begin %o %s", &mode, dest);

	/* handle ~user/file format */
	if (dest[0] == '~') {
		char *sl;
		struct passwd *getpwnam();
		struct passwd *user;
		char dnbuf[100], *index(), *strcat(), *strcpy();

		sl = index(dest, '/');
		if (sl == NULL) {
			fprintf(stderr, "Illegal ~user\n");
			exit(3);
		}
		*sl++ = 0;
		user = getpwnam(dest+1);
		if (user == NULL) {
			fprintf(stderr, "No such user as %s\n", dest);
			exit(4);
		}
		strcpy(dnbuf, user->pw_dir);
		strcat(dnbuf, "/");
		strcat(dnbuf, sl);
		strcpy(dest, dnbuf);
	}

	/* create output file */
	out = fopen(dest, "w");
	if (out == NULL) {
		perror(dest);
		exit(4);
	}
	chmod(dest, mode);

	decode(in, out);

	if (fgets(buf, sizeof buf, in) == NULL || strcmp(buf, "end\n")) {
		fprintf(stderr, "No end line\n");
		exit(5);
	}
	exit(0);
}

/*
 * copy from in to out, decoding as you go along.
 */
decode(in, out)
FILE *in;
FILE *out;
{
	char buf[80];
	char *bp;
	int n;

	for (;;) {
		/* for each input line */
		if (fgets(buf, sizeof buf, in) == NULL) {
			printf("Short file\n");
			exit(10);
		}
		n = DEC(buf[0]);
		if (n <= 0)
			break;

		bp = &buf[1];
		while (n > 0) {
			outdec(bp, out, n);
			bp += 4;
			n -= 3;
		}
	}
}

/*
 * output a group of 3 bytes (4 input characters).
 * the input chars are pointed to by p, they are to
 * be output to file f.  n is used to tell us not to
 * output all of them at the end of the file.
 */
outdec(p, f, n)
char *p;
FILE *f;
{
	int c1, c2, c3;

	c1 = DEC(*p) << 2 | DEC(p[1]) >> 4;
	c2 = DEC(p[1]) << 4 | DEC(p[2]) >> 2;
	c3 = DEC(p[2]) << 6 | DEC(p[3]);
	if (n >= 1)
		putc(c1, f);
	if (n >= 2)
		putc(c2, f);
	if (n >= 3)
		putc(c3, f);
}
