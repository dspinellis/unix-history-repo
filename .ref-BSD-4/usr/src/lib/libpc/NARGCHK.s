# Copyright (c) 1979 Regents of the University of California
#
# sccsid[] = "@(#)NARGCHK.s 1.1 10/29/80";
#
	.align	1
	.globl	_NARGCHK
_NARGCHK:
	.word	0
	cmpl	4(ap),*AP(fp)
	bneq	L1
	ret
L1:
	subl3	4(ap),*AP(fp),-(sp)
	pushl	$ENARGS
	calls	$2,_ERROR
	ret
