/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)whereis.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/sysctl.h>

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	struct stat sb;
	size_t len;
	int ch, sverrno, mib[2];
	char *p, *t, *std, path[MAXPATHLEN];

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch (ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	/* Retrieve the standard path. */
	mib[0] = CTL_USER;
	mib[1] = USER_CS_PATH;
	if (sysctl(mib, 2, NULL, &len, NULL, 0) == -1)
		return (-1);
	if (len == 0)
		err(1, "user_cs_path: sysctl: zero length\n");
	if ((std = malloc(len)) == NULL)
		err(1, NULL);
	if (sysctl(mib, 2, std, &len, NULL, 0) == -1) {
		sverrno = errno;
		free(std);
		errno = sverrno;
		err(1, "sysctl: user_cs_path");
	}

	/* For each path, for each program... */
	for (; *argv; ++argv)
		for (p = std;; *p++ = ':') {
			t = p;
			if ((p = strchr(p, ':')) != NULL) {
				*p = '\0';
				if (t == p)
					t = ".";
			} else
				if (strlen(t) == 0)
					t = ".";
			(void)snprintf(path, sizeof(path), "%s/%s", t, *argv);
			if (!stat(path, &sb))
				(void)printf("%s\n", path);
			if (p == NULL)
				break;
		}
}

void
usage()
{
	(void)fprintf(stderr, "whereis: program ...\n");
	exit (1);
}
