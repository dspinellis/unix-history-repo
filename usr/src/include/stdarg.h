/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stdarg.h	5.2 (Berkeley) %G%
 */

typedef char *va_list;

/*
 * ANSI says: "If there is no actual next argument, or if type is not
 * compatible with the type of the actual next argument (as promoted
 * according to the default argument promotions), the behavior is
 * undefined."  We read this to mean that we're not allowed to do the
 * promotion for the user, so shorts and chars drop core.
 */
#define	va_arg(ap, type) \
	((type *)(ap += sizeof(type) < sizeof(int) ? \
		abort() : sizeof(type)))[-1]

#define	va_end(ap)

#define	__va_promote(type) \
	(((sizeof(type) + sizeof(int) - 1) / sizeof(int)) * sizeof(int))

#define	va_start(ap, last) \
	(ap = ((char *)&(last) + __va_promote(last)))
