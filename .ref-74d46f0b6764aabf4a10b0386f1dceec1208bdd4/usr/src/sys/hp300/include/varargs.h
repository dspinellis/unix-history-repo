/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)varargs.h	5.3 (Berkeley) %G%
 */

#ifndef _VARARGS_H_
#define	_VARARGS_H_

#include <stdarg.h>

#undef	va_dcl
#define	va_dcl	int va_alist;

#undef	va_start
#define	va_start(ap) \
	ap = (char *)&va_alist

#endif /* !_VARARGS_H_ */
