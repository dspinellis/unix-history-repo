	# optimized (and corrected) from output of C compiler
	#	-16(fp) --> r0
	#	-20(fp) --> r8
	#	-8(fp) --> r6
	#	-21(fp) --> r5
	#	-22(fp) --> r4
	.set	one,010				# 1.0 in floating immediate format
	.set	ten,042				# 10.0 in floating immediate format
big:	.word	0x5c80,0,0,0		# 2**56 in double floating
huge:	.word	0x8000,0,0,0		# reserved operand
L49:	.double	0d1.00000000000000000000e+21
	.set	.R1,0xff0
	.set	.F1,24
	.globl	_atof
_atof:
	.word	.R1
	movl	4(ap),r11
L15:
	cvtbl	(r11)+,r10
	cmpl	r10,$32
	jeql	L15
	clrb	r5
	cmpb	r10,$45
	jneq	L17
	incb	r5
	jbr	L18
L17:
	cmpb	r10,$43
	jeql	L18
	decl	r11
L18:
	clrd	r0
	clrl	r9
	jbr	L22
L20001:
	muld2	$ten,r0
	subl3	$48,r10,r6
	cvtld	r6,r6
	addd2	r6,r0
	jbr	L22
L20003:
	cmpb	r10,$48
	jlss	L23
	cmpd	r0,big
	jlss	L20001
	incl	r9
L22:
	cvtbl	(r11)+,r10
	cmpl	r10,$57
	jleq	L20003
L23:
	cmpb	r10,$46
	jeql	L28
	jbr	L27
L20005:
	cmpb	r10,$48
	jlss	L27
	cmpd	r0,big
	jgeq	L28
	decl	r9
	muld2	$ten,r0
	subl3	$48,r10,r6
	cvtld	r6,r6
	addd2	r6,r0
L28:
	cvtbl	(r11)+,r10
	cmpl	r10,$57
	jleq	L20005
L27:
	tstb	r5
	jeql	L8827
	mnegd	r0,r0
L8827:
	clrl	r8
	cmpb	r10,$69
	jeql	L10000
	cmpb	r10,$101
	jneq	L32
L10000:
	clrb	r4
	cvtbl	(r11)+,r10
	cmpl	r10,$45
	jneq	L33
	incb	r4
	jbr	L37
L33:
	cmpb	r10,$43
	jeql	L37
	decl	r11
	jbr	L37
L20007:
	cmpb	r10,$48
	jlss	L38
	moval	(r8)[r8],r8
	movaw	-48(r10)[r8],r8
L37:
	cvtbl	(r11)+,r10
	cmpl	r10,$57
	jleq	L20007
L38:
	tstb	r4
	jeql	L32
	mnegl	r8,r8
L32:
	addl2	r8,r9
	jneq	L40
	ret
L40:
	clrb	r4
	tstl	r9
	jgeq	L41
	incb	r4
	mnegl	r9,r9
L41:
	cmpl	r9,$38
	jleq	L42
	tstb	r4
	jeql	L43
	clrd	r0
	ret
L43:
	tstb	r5
	jeql	L8843
	mnegd	huge,r0
	ret
L8843:
	movd	huge,r0
	ret
L42:
	movd	$one,r6
	jbr	L46
L20009:
	divd2	r6,r0
	ret
L48:
	muld2	$ten,r6
	decl	r9
L46:
	tstl	r9
	jeql	L47
	cmpl	r9,$21
	jneq	L48
	muld2	L49,r6
L47:
	tstb	r4
	jneq	L20009
	muld2	r6,r0
	ret
