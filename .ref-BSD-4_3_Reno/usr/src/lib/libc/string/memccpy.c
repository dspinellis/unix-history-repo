/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)memccpy.c	5.6 (Berkeley) 5/29/90";
#endif /* LIBC_SCCS and not lint */

#include <string.h>
#include <sys/stdc.h>

void *
memccpy(t, f, c, n)
	void *t;
	const void *f;
	int c;
	register size_t n;
{

	if (n) {
		register unsigned char *t;
		register const unsigned char *f;
		register unsigned char ch = c;

		do {
			if ((*t++ = *f++) == c)
				return (t);
		} while (--n != 0);
	}
	return (0);
}
