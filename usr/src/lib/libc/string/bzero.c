/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)bzero.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <string.h>

/*
 * bzero -- vax movc5 instruction
 */
void
bzero(b, length)
	void *b;
	register size_t length;
{
	register char *p;

	for (p = b; length--;)
		*p++ = '\0';
}
