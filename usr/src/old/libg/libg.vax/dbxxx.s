#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#ifndef lint
_sccsid:.asciz	"@(#)dbxxx.s	5.1 (Berkeley) %G%"
#endif not lint

	.data
	.comm	__dbargs,512
	.text
	.align	1
	.globl	__dbsubc
__dbsubc:
	callg	__dbargs+4,*__dbargs
	.globl	__dbsubn
__dbsubn:
	halt

	.data
