/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1990, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)makekey.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static void get __P((char *, int));

int
main()
{
	int len;
	char *r, key[9], salt[3];
	
	get(key, sizeof(key) - 1);
	get(salt, sizeof(salt) - 1);
	len = strlen(r = crypt(key, salt));
	if (write(STDOUT_FILENO, r, len) != len)
		err(1, "stdout");
	exit(0);
}

static void
get(bp, len)
	char *bp;
	register int len;
{
	register int nr;

	bp[len] = '\0';
	if ((nr = read(STDIN_FILENO, bp, len)) == len)
		return;
	if (nr >= 0)
		errno = EFTYPE;
	err(1, "stdin");
}
