/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strrchr.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <string.h>

char *
strrchr(p, ch)
	char *p, ch;
{
	return(rindex(p, ch));
}
