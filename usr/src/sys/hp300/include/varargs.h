/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)varargs.h	5.1 (Berkeley) %G%
 */

typedef char *va_list;

#define	__va_round(type) \
	(((sizeof(type) + sizeof(int) - 1) / sizeof(int)) * sizeof(int))

#define	va_arg(ap, type) \
	((type *)(ap += __va_round(type)))[-1]
#define	va_dcl	int va_alist;
#define	va_end(ap)
#define	va_start(ap) \
	ap = (char *)&va_alist
