/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cosh.s	8.1 (Berkeley) %G%
 */

/* cosh(x) */

	.text
	.globl	_cosh

_cosh:
	fcoshd	sp@(4),fp0
	fmoved	fp0,sp@-
	movel	sp@+,d0
	movel	sp@+,d1
	rts
