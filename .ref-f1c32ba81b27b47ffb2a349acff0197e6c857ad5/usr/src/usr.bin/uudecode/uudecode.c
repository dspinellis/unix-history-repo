/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)uudecode.c	5.10 (Berkeley) %G%";
#endif /* not lint */

/*
 * uudecode [file ...]
 *
 * create the specified file, decoding as you go.
 * used with uuencode.
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <pwd.h>
#include <stdio.h>
#include <string.h>

char *filename;

/* ARGSUSED */
main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno;
	int rval;

	if (*++argv) {
		rval = 0;
		do {
			if (!freopen(filename = *argv, "r", stdin)) {
				(void)fprintf(stderr, "uudecode: %s: %s\n",
				    *argv, strerror(errno));
				rval = 1;
				continue;
			}
			rval |= decode();
		} while (*++argv);
	} else {
		filename = "stdin";
		rval = decode();
	}
	exit(rval);
}

decode()
{
	extern int errno;
	struct passwd *pw;
	register int n;
	register char ch, *p;
	int mode, n1;
	char buf[MAXPATHLEN];

	/* search for header line */
	do {
		if (!fgets(buf, sizeof(buf), stdin)) {
			(void)fprintf(stderr,
			    "uudecode: %s: no \"begin\" line\n", filename);
			return(1);
		}
	} while (strncmp(buf, "begin ", 6));
	(void)sscanf(buf, "begin %o %s", &mode, buf);

	/* handle ~user/file format */
	if (buf[0] == '~') {
		if (!(p = index(buf, '/'))) {
			(void)fprintf(stderr, "uudecode: %s: illegal ~user.\n",
			    filename);
			return(1);
		}
		*p++ = NULL;
		if (!(pw = getpwnam(buf + 1))) {
			(void)fprintf(stderr, "uudecode: %s: no user %s.\n",
			    filename, buf);
			return(1);
		}
		n = strlen(pw->pw_dir);
		n1 = strlen(p);
		if (n + n1 + 2 > MAXPATHLEN) {
			(void)fprintf(stderr, "uudecode: %s: path too long.\n",
			    filename);
			return(1);
		}
		bcopy(p, buf + n + 1, n1 + 1);
		bcopy(pw->pw_dir, buf, n);
		buf[n] = '/';
	}

	/* create output file, set mode */
	if (!freopen(buf, "w", stdout) ||
	    fchmod(fileno(stdout), mode&0666)) {
		(void)fprintf(stderr, "uudecode: %s: %s: %s\n", buf,
		    filename, strerror(errno));
		return(1);
	}

	/* for each input line */
	for (;;) {
		if (!fgets(p = buf, sizeof(buf), stdin)) {
			(void)fprintf(stderr, "uudecode: %s: short file.\n",
			    filename);
			return(1);
		}
#define	DEC(c)	(((c) - ' ') & 077)		/* single character decode */
		/*
		 * `n' is used to avoid writing out all the characters
		 * at the end of the file.
		 */
		if ((n = DEC(*p)) <= 0)
			break;
		for (++p; n > 0; p += 4, n -= 3)
			if (n >= 3) {
				ch = DEC(p[0]) << 2 | DEC(p[1]) >> 4;
				putchar(ch);
				ch = DEC(p[1]) << 4 | DEC(p[2]) >> 2;
				putchar(ch);
				ch = DEC(p[2]) << 6 | DEC(p[3]);
				putchar(ch);
			}
			else {
				if (n >= 1) {
					ch = DEC(p[0]) << 2 | DEC(p[1]) >> 4;
					putchar(ch);
				}
				if (n >= 2) {
					ch = DEC(p[1]) << 4 | DEC(p[2]) >> 2;
					putchar(ch);
				}
				if (n >= 3) {
					ch = DEC(p[2]) << 6 | DEC(p[3]);
					putchar(ch);
				}
			}
	}
	if (!fgets(buf, sizeof(buf), stdin) || strcmp(buf, "end\n")) {
		(void)fprintf(stderr, "uudecode: %s: no \"end\" line.\n",
		    filename);
		return(1);
	}
	return(0);
}

usage()
{
	(void)fprintf(stderr, "usage: uudecode [file ...]\n");
	exit(1);
}
