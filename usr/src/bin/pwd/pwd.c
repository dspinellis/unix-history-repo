/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pwd.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

main()
{
	char *p;

	p = getcwd((char *)NULL, 0);
	if (p) {
		(void)printf("%s\n", p);
		exit(0);
	}
	(void)fprintf(stderr, "pwd: %s\n", strerror(errno));
	exit(1);
}
