/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)varargs.h	5.4 (Berkeley) %G%
 */

#ifndef _VARARGS_H_
#define	_VARARGS_H_

typedef char *va_list;

#define	va_dcl	int va_alist;

#define	va_start(ap) \
	ap = (char *)&va_alist

#ifdef KERNEL
#define	va_arg(ap, type) \
	((type *)(ap += sizeof(type)))[-1]
#else
#define	va_arg(ap, type) \
	((type *)(ap += sizeof(type) < sizeof(int) ? \
		(abort(), 0) : sizeof(type)))[-1]
#endif

#define	va_end(ap)

#endif /* !_VARARGS_H_ */
