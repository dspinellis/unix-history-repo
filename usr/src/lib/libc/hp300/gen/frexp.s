/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)frexp.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/*
 * double frexp(val, eptr)
 * returns: x s.t. val = x * (2 ** n), with n stored in *eptr
 */
ENTRY(frexp)
	fmoved		sp@(4),fp1
	fgetmanx	fp1,fp0
	fgetexpx	fp1
	fmovel		fp1,d0
	addql		#1,d0
	movel		sp@(12),a0
	movel		d0,a0@
	fdivl		#2,fp0
	fmoved		fp0,sp@-
	movel		sp@+,d0
	movel		sp@+,d1
	rts
