/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stdarg.h	5.4 (Berkeley) %G%
 */

typedef char *va_list;

#define	va_arg(ap, type) \
	((type *)(ap += sizeof(type) < sizeof(int) ? \
		(abort(), 0) : sizeof(type)))[-1]

#define	va_end(ap)

#define	__va_promote(type) \
	(((sizeof(type) + sizeof(int) - 1) / sizeof(int)) * sizeof(int))

#define	va_start(ap, last) \
	(ap = ((char *)&(last) + __va_promote(last)))
