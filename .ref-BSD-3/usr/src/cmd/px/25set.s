#
# 25set.s
#
# SET OPERATIONS
#
_ADDT:
	cvtbl	(r10)+,r0
	bneq	l2501
	cvtwl	(r10)+,r0
l2501:
	blbc	r0,l2502
	incl	r0		#r0 has number of bytes in set
l2502:
	addl3	sp,r0,r1	#r1 has pointer to second set
	ashl	$-1,r0,r0	#r0 has number of words in set
l2503:
	bisw2	(sp)+,(r1)+
	sobgtr	r0,l2503
	jmp	(r8)
_SUBT:
	cvtbl	(r10)+,r0
	bneq	l2504
	cvtwl	(r10)+,r0
l2504:
	blbc	r0,l2505
	incl	r0		#r0 has number of bytes in set
l2505:
	addl3	sp,r0,r1	#r1 has pointer to second set
	ashl	$-1,r0,r0	#r0 has number of words in set
l2506:
	bicw2	(sp)+,(r1)+
	sobgtr	r0,l2506
	bicw2	(r10)+,-(r1)
	jmp	(r8)
_MULT:
	cvtbl	(r10)+,r0
	bneq	l2507
	cvtwl	(r10)+,r0
l2507:
	blbc	r0,l2508
	incl	r0		#r0 has number of bytes in set
l2508:
	addl3	sp,r0,r1	#r1 has pointer to second set
	ashl	$-1,r0,r0	#r0 has number of words in set
l2509:
	mcomw	(sp)+,r3
	bicw2	r3,(r1)+
	sobgtr	r0,l2509
	jmp	(r8)
_CARD:
	cvtbl	(r10)+,r0	#r0 has number of bytes in set
	bneq	l2510
	cvtwl	(r10)+,r0
l2510:
	blbc	r0,l2511
	incl	r0
l2511:
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
	cvtbl	(r10)+,-(sp)
	bneq	l2513
	cvtwl	(r10)+,(sp)
l2513:
	cvtwl	(r10)+,-(sp)
	cvtwl	(r10)+,-(sp)
	calls	$4,_pcttot
	movw	r0,sp
	jmp	(r8)
_IN:
	cvtbl	(r10)+,r0
	bneq	l2514
	cvtwl	(r10)+,r0	#r0 has size of set
l2514:
	blbc	r0,l2515
	incl	r0
l2515:
	cvtwl	(sp)+,r1	#r1 has set index
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
	incl	r10
	cvtwl	(sp)+,r0	#r0 has value to find
	cvtwl	(sp)+,r1	#r1 has pair count
l2518:
	cmpw	r0,(sp)+
	blss	l2519
	cmpw	r0,(sp)+
	bgtr	l2520
	decl	r1
	moval	(sp)[r1],sp	#clear off remaining data on stack
	movw	$1,-(sp)	#success
	jmp	(r8)
l2519:
	addl2	$2,sp
l2520:
	sobgtr	r1,l2518
	clrw	-(sp)		#failure
	jmp	(r8)
