/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)putenv.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdlib.h>
#include <string.h>

int
putenv(str)
	char *str;
{
	register char *p, *equal;
	int rval;

	if (!(p = strdup(str)))
		return(1);
	if (!(equal = index(p, '='))) {
		(void)free(p);
		return(1);
	}
	*equal = '\0';
	rval = setenv(p, equal + 1, 1);
	(void)free(p);
	return(rval);
}
