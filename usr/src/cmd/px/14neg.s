#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)14neg.s 4.1 10/10/80";
#
# NEGATION & ABSOLUTE VALUE
#
_NEG2:
	incl	r10
	cvtwl	(sp)+,r0
l1401:
	mnegl	r0,-(sp)
	jmp	(r8)
_ABS2:
_ABS4:
	incl	r10
	tstl	(sp)
	jgeq	l1402
	mnegl	(sp),(sp)
l1402:
	jmp	(r8)
_NEG4:
	incl	r10
	mnegl	(sp),(sp)
	jmp	(r8)
_ABS8:
	incl	r10
	tstd	(sp)
	jgeq	l1403
	mnegd	(sp),(sp)
l1403:
	jmp	(r8)
_NEG8:
	incl	r10
	mnegd	(sp),(sp)
	jmp	(r8)
