/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */
#ifndef lint
_sccsid:.asciz	"@(#)dbxxx.c	5.3 (Berkeley) 1/26/86"
#endif not lint

#if defined(vax)
#define	CALL(n, f)	callg	n,*f
#endif
#if defined(tahoe)
#define	CALL(n, f)	calls	n,*f
#endif

	.data
	.comm	__dbargs,512
	.text
	.align	1
	.globl	__dsubc
__dsubc:
	CALL(__dbargs+4,__dbargs)
	.globl	__dsubn
__dsubn:
	halt

	.data
/*  __lg_flag is used by f77_abort to decide whether or not to dump memory */
	.globl	__lg_flag
__lg_flag:
	.long	1
	.text
