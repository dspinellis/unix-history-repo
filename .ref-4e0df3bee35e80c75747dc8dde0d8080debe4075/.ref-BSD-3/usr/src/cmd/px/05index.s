#
# 05index.s
#
# OFF, INDEX and NIL
#
_OFF:
	cvtbl	(r10)+,r0
	bneq	l0501
	cvtwl	(r10)+,r0
l0501:
	addl2	r0,(sp)
	jmp	(r8)
_INX2:
	cvtbl	(r10)+,r0
	bneq	l0502
	cvtwl	(r10)+,r0	#r0 has size
l0502:
	cvtwl	(r10)+,r1	#r1 has lower bound
	cvtwl	(r10)+,r2	#r2 has upper bound
	cvtwl	(sp)+,r3	#r3 contains subscript
	subl2	r1,r3		#r3 has base subscript
	index	r3,$0,r2,r0,$0,r1  #r1 has calculated offset
	addl2	r1,(sp)		#calculate actual address
	jmp	(r8)
_INX4:
	cvtbl	(r10)+,r0
	bneq	l0503
	cvtwl	(r10)+,r0	#r0 has size
l0503:
	cvtwl	(r10)+,r1	#r1 has lower bound
	cvtwl	(r10)+,r2	#r2 has upper bound
	movl	(sp)+,r3	#r3 contains subscript
	subl2	r1,r3		#r3 has base subscript
	index	r3,$0,r2,r0,$0,r1  #r1 has calculated offset
	addl2	r1,(sp)		#calculate actual address
	jmp	(r8)
_NIL:
	incl	r10
	tstl	(sp)
	jeql	l0504
	jmp	(r8)
l0504:
	movw	$ENILPTR,_perrno
	jbr	error
_INX4P2:
	cvtbl	(r10)+,r0	#r0 has shift amount
	cvtwl	(r10)+,r2	#r2 has lower bound
	subl3	r2,(sp)+,r1	#r1 has base subscript
	ashl	r0,r1,r1
	addl2	r1,(sp)
	jmp	(r8)
_INX2P2:
	cvtbl	(r10)+,r0	#r0 has shift amount
	clrl	r1		#clear upper half of r1
	subw3	(r10)+,(sp)+,r1	#r1 has base subscript
	ashl	r0,r1,r1
	addl2	r1,(sp)
	jmp	(r8)
