/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stdarg.h	7.3 (Berkeley) %G%
 */

#ifndef _STDARG_H_
#define	_STDARG_H_

typedef char *va_list;

#define	__va_promote(type) \
	(((sizeof(type) + sizeof(int) - 1) / sizeof(int)) * sizeof(int))

#define	va_start(ap, last) \
	(ap = ((char *)&(last) + __va_promote(last)))

#ifdef KERNEL
#define	va_arg(ap, type) \
	((type *)(ap += sizeof(type)))[-1]
#else
#define	va_arg(ap, type) \
	((type *)(ap += sizeof(type) == sizeof(int) ? sizeof(type) : \
		sizeof(type) > sizeof(int) ? \
		(-(int)(ap) & (sizeof(type) - 1)) + sizeof(type) : \
		(abort(), 0)))[-1]
#endif

#define	va_end(ap)

#endif /* !_STDARG_H_ */
