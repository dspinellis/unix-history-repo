/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)varargs.h	7.1 (Berkeley) %G%
 *
 * from: $Header: varargs.h,v 1.4 92/06/17 06:10:31 torek Exp $
 */

#ifndef _MACHINE_VARARGS_H_
#define	_MACHINE_VARARGS_H_

typedef char *va_list;
#define	va_dcl	int va_alist;
#define	va_start(ap) (__builtin_saveregs(), (ap) = (char *)&va_alist)
#define va_arg(ap, t)	(((t *)(ap += sizeof(t)))[-1])
#define va_end(ap) /* empty */

#endif /* !_MACHINE_VARARGS_H_ */
