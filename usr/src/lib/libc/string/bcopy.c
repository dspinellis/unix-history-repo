/*
 * Copyright (c) 1987, 1989 Regents of the University of California.
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
static char sccsid[] = "@(#)bcopy.c	5.6 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

/*
 * bcopy -- copy memory block, handling overlap of source and destination
 *	(vax movc3 instruction)
 */

typedef int word;		/* size of "word" used for optimal copy speed */

bcopy(src, dst, length)
	register char *src, *dst;
	register int length;
{
	if (length && src != dst)
		if ((u_int)dst < (u_int)src)
			if (((int)src | (int)dst | length) & (sizeof(word) - 1))
				do	/* copy by bytes */
					*dst++ = *src++;
				while (--length);
			else {
				length /= sizeof(word);
				do {	/* copy by words */
					*(word *)dst = *(word *)src;
					src += sizeof(word);
					dst += sizeof(word);
				} while (--length);
			}
		else {			/* copy backwards */
			src += length;
			dst += length;
			if (((int)src | (int)dst | length) & (sizeof(word) - 1))
				do	/* copy by bytes */
					*--dst = *--src;
				while (--length);
			else {
				length /= sizeof(word);
				do {	/* copy by words */
					src -= sizeof(word);
					dst -= sizeof(word);
					*(word *)dst = *(word *)src;
				} while (--length);
			}
		}
}
