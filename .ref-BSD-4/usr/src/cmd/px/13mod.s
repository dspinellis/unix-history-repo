#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)13mod.s 4.1 10/10/80";
#
# MODULO
#
_MOD2:
	incl	r10
	cvtwl	(sp),r0
	cvtwl	2(sp),r2
	ashq	$-32,r1,r1
	ediv	r0,r1,r3,(sp)
	jmp	(r8)
_MOD24:
	incl	r10
	cvtwl	(sp)+,r0
	movl	(sp),r2
	ashq	$-32,r1,r1
	ediv	r0,r1,r3,(sp)
	jmp	(r8)
_MOD42:
	incl	r10
	movl	(sp)+,r0
	cvtwl	(sp)+,r2
	ashq	$-32,r1,r1
	ediv	r0,r1,r3,-(sp)
	jmp	(r8)
_MOD4:
	incl	r10
	movl	(sp)+,r0
	movl	(sp),r2
	ashq	$-32,r1,r1
	ediv	r0,r1,r3,(sp)
	jmp	(r8)
