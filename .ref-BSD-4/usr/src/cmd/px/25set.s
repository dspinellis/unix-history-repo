#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)25set.s 4.1 10/10/80";
#
# SET OPERATIONS
#
_ADDT:
	cvtbl	(r10)+,r0
	bneq	l2501
	movzwl	(r10)+,r0
l2501:
	addl3	sp,r0,r1	#r1 has pointer to second set
	ashl	$-2,r0,r4	#r4 has number of longs in set
l2503:
	bisl2	(sp)+,(r1)+
	sobgtr	r4,l2503
	jmp	(r8)
_SUBT:
	cvtbl	(r10)+,r0
	bneq	l2504
	movzwl	(r10)+,r0
l2504:
	addl3	sp,r0,r1	#r1 has pointer to second set
	ashl	$-2,r0,r4	#r4 has number of longs in set
l2506:
	bicl2	(sp)+,(r1)+
	sobgtr	r4,l2506
	jmp	(r8)
_MULT:
	cvtbl	(r10)+,r0
	bneq	l2507
	movzwl	(r10)+,r0
l2507:
	addl3	sp,r0,r1	#r1 has pointer to second set
	ashl	$-2,r0,r4	#r4 has number of longs in set
l2509:
	mcoml	(sp)+,r3
	bicl2	r3,(r1)+
	sobgtr	r4,l2509
	jmp	(r8)
_CARD:
	cvtbl	(r10)+,r0	#r0 has number of bytes in set
	bneq	l2510
	movzwl	(r10)+,r0
l2510:
	blbc	r0,l2511
	incl	r0
l2511:
	cmpl	r0,$2		#check for long align
	bneq	l2525
	addl2	$2,r0		#if so append it
l2525:
	addl3	r0,sp,r4	#r4 has new stack addr
	ashl	$3,r0,r0	#r0 has number of bits in set
	mnegl	$1,r1		#will init r1 to zero
	mnegl	$1,r5		#will init r2 to zero
l2512:
	incl	r1		#count found element
	incl	r5		#advance to next field position
l2521:
	ffs	r5,$32,(sp),r5	#find next set bit
	beql	l2521		#nothing found, so continue
	cmpl	r5,r0		#check for end of field
	blss	l2512		#element found, so count and continue
	movl	r4,sp		#clear stack
	movw	r1,-(sp)	#put answer on stack
	jmp	(r8)
_CTTOT:
	cvtbl	(r10)+,r0
	bneq	l2513
	movzwl	(r10)+,r0
l2513:
	pushal	-4(sp)[r0]
	calls	r0,_cttot
	jmp	(r8)
_IN:
	cvtbl	(r10)+,r0
	bneq	l2514
	movzwl	(r10)+,r0	#r0 has size of set
l2514:
	movl	(sp)+,r1	#r1 has set index
	addl3	r0,sp,r4	#r4 points to new top of stack
	subw2	(r10)+,r1	#check below lower
	blssu	l2516
	cmpw	r1,(r10)+	#check above upper
	bgtru	l2517
	bbc	r1,(sp),l2517	#check for bit set
	movl	r4,sp		#bit found
	movw	$1,-(sp)
	jmp	(r8)
l2516:
	addl2	$2,r10
l2517:
	movl	r4,sp		#bit not found
	clrw	-(sp)
	jmp	(r8)
_INCT:
	cvtbl	(r10)+,r0
	bneq	l2518
	movzwl	(r10)+,r0	#r0 has number of elements
l2518:
	calls	r0,_inct
	movw	r0,-(sp)
	jmp	(r8)
