/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strcoll.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <string.h>
#include <sys/stdc.h>

/*
 * Compare strings according to LC_COLLATE category of current locale.
 */
strcoll(s1, s2)
	const char *s1, *s2;
{
	/* LC_COLLATE is unimplemented, hence always "C" */
	return (strcmp(s1, s2));
}
