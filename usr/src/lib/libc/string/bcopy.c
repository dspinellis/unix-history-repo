/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)bcopy.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

/*
 * bcopy -- vax movc3 instruction
 */
bcopy(src, dst, length)
	register long *src, *dst;
	register int length;
{
	if (length && src != dst)
		if ((u_int)dst < (u_int)src)
			if (((int)src | (int)dst | length) & 3)
				do	/* copy by bytes */
					*dst++ = *src++;
				while (--length);
			else {
				length >>= 2;
				do	/* copy by longs */
					*((long *)dst)++ = *((long *)src)++;
				while (--length);
			}
		else {			/* copy backwards */
			src += length;
			dst += length;
			if (((int)src | (int)dst | length) & 3)
				do	/* copy by bytes */
					*--dst = *--src;
				while (--length);
			else {
				length >>= 2;
				do	/* copy by shorts */
					*--((long *)dst) = *--((long *)src);
				while (--length);
			}
		}
}
