/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)putenv.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdlib.h>

putenv(str)
	char *str;
{
	register char *equal;
	int rval;

	if (!(equal = index(str, '=')))
		return(1);
	*equal = '\0';
	rval = setenv(str, equal + 1, 1);
	*equal = '=';
	return(rval);
}
