/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)varargs.h	8.3 (Berkeley) %G%
 *
 * from: $Header: varargs.h,v 1.8 93/09/27 00:53:20 torek Exp $
 */

#ifndef _MACHINE_VARARGS_H_
#define	_MACHINE_VARARGS_H_

typedef char *va_list;

/* See <machine/stdarg.h> for comments. */
#if __GNUC__ == 1
#define __extension__
#define	va_dcl	int va_alist;
#else /* gcc2 */
#ifdef __GCC_NEW_VARARGS__	/* gcc 2.4.5 */
#define va_alist __builtin_va_alist
#define	va_dcl	int __builtin_va_alist;
#else				/* gcc 2.3.3 */
#define	va_dcl	int va_alist; ...
#endif
#endif

#ifdef __GCC_NEW_VARARGS__
#define	va_start(ap)	((ap) = (char *)__builtin_saveregs())
#else
#define	va_start(ap)	(__builtin_saveregs(), (ap) = (char *)&va_alist)
#endif
#define va_end(ap)	/* empty */

/* Note, we can assume C code here; C++ does not use <varargs.h>. */
#define	__va_8byte(ap, ty) ({ \
	union { ty __d; int __i[2]; } __va_u; \
	__va_u.__i[0] = ((int *)(void *)(ap))[0]; \
	__va_u.__i[1] = ((int *)(void *)(ap))[1]; \
	(ap) += 8; __va_u.__d; })
#define va_arg(ap, ty) __extension__ ({ \
    ty __va_temp; \
    __builtin_classify_type(__va_temp) >= 12 ? \
	((ty **)(void *)((ap) += sizeof(ty *)))[-1][0] : \
    sizeof(ty) == 8 ? __va_8byte(ap, ty) : \
	((ty *)(void *)(ap += sizeof(ty)))[-1]; })

#endif /* !_MACHINE_VARARGS_H_ */
