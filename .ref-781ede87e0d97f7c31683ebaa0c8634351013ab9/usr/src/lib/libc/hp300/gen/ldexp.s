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
	.asciz "@(#)ldexp.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/*
 * double ldexp(val, exp)
 * returns: val * (2**exp), for integer exp
 */
ENTRY(ldexp)
	fmoved		sp@(4),fp0
	fbeq		Ldone
	ftwotoxl	sp@(12),fp1
	fmulx		fp1,fp0
Ldone:
	fmoved		fp0,sp@-
	movel		sp@+,d0
	movel		sp@+,d1
	rts
