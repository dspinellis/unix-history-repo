#
# Copyright (c) 1985 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
# All recipients should regard themselves as participants in an ongoing
# research project and hence should feel obligated to report their
# experiences (good or bad) with these elementary function codes, using
# the sendbug(8) program, to the authors.
#
#	@(#)infnan.s	5.3 (Berkeley) %G%
#
	.data
	.align	2
_sccsid:
.asciz	"@(#)infnan.s	5.3	(ucb.elefunt)	%G%"

/*
 * double infnan(arg)
 * int arg;
 * where arg :=    EDOM	if result is  NaN
 *	     :=  ERANGE	if result is +INF
 *	     := -ERANGE if result is -INF
 *
 * The Reserved Operand Fault is generated inside of this routine.
 */	
	.globl	_infnan
	.set	EDOM,33
	.set	ERANGE,34
	.text
	.align 2
_infnan:
	.word	0x0000			# save nothing
	cmpl	4(fp),$ERANGE
	bneq	1f
	movl	$ERANGE,_errno
	brb	2f
1:	movl	$EDOM,_errno
2:	cmpf2	$0x80000000,$0x80000000	# generates the reserved operand fault
	ret
