/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)varargs.h	8.2 (Berkeley) %G%
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
