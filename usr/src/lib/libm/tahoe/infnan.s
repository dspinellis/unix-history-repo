/* 
 * Copyright (c) 1985 Regents of the University of California.
 * 
 * Use and reproduction of this software are granted  in  accordance  with
 * the terms and conditions specified in  the  Berkeley  Software  License
 * Agreement (in particular, this entails acknowledgement of the programs'
 * source, and inclusion of this notice) with the additional understanding
 * that  all  recipients  should regard themselves as participants  in  an
 * ongoing  research  project and hence should  feel  obligated  to report
 * their  experiences (good or bad) with these elementary function  codes,
 * using "sendbug 4bsd-bugs@BERKELEY", to the authors.
 */
	.data
	.align	2
_sccsid:
.asciz	"@(#)infnan.s	5.2	(ucb.elefunt)	%G%"

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
