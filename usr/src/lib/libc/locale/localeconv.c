/*
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)localeconv.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <locale.h>

/*
 * Return the current locale conversion.
 */
struct lconv *
localeconv()
{
	extern struct lconv *__lconv;

	return (__lconv);
}
