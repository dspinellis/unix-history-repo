/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)makekey.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

main()
{
	int len;
	char *r, key[9], salt[3], *crypt();
	
	get(key, sizeof(key) - 1);
	get(salt, sizeof(salt) - 1);
	len = strlen(r = crypt(key, salt));
	if (write(STDOUT_FILENO, r, len) != len)
		error();
	exit(0);
}

static
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
	error();
}

static
error()
{
	(void)fprintf(stderr, "makekey: %s\n", strerror(errno));
	exit(1);
}
