/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)varargs.h	7.3 (Berkeley) %G%
 *
 * from: $Header: varargs.h,v 1.5 92/10/16 04:16:09 torek Exp $
 */

#ifndef _MACHINE_VARARGS_H_
#define	_MACHINE_VARARGS_H_

/* See <machine/stdarg.h> for comments. */
#if __GNUC__ == 1
#define __extension__
#endif
typedef char *va_list;
#define	va_dcl	int va_alist;
#define	va_start(ap) (__builtin_saveregs(), (ap) = (char *)&va_alist)
#define va_arg(ap, ty) \
    (sizeof(ty) == 8 ? __extension__ ({ \
	union { ty __d; int __i[2]; } __u; \
	__u.__i[0] = ((int *)(ap))[0]; \
	__u.__i[1] = ((int *)(ap))[1]; \
	(ap) += 8; \
	__u.__d; }) : \
    ((ty *)(ap += sizeof(ty)))[-1])
#define va_end(ap) /* empty */

#endif /* !_MACHINE_VARARGS_H_ */
