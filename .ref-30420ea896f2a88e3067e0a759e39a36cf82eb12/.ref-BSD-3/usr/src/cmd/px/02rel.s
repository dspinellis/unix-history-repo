#
# 02rel.s
#
# RELATIONAL OPERATORS
#
_REL2:
	cvtbl	(r10)+,r0
	ashl	$1,r0,r0	#maintain compatability
	movw	(sp)+,r1
	cmpw	(sp)+,r1
	jmp	*reltab(r0)
_REL42:
	cvtbl	(r10)+,r0
	ashl	$1,r0,r0	#maintain compatability
	movl	(sp)+,r1
	cvtwl	(sp)+,r2
	cmpl	r2,r1
	jmp	*reltab(r0)
_REL24:
	cvtbl	(r10)+,r0
	ashl	$1,r0,r0	#maintain compatability
	cvtwl	(sp)+,r1
	cmpl	(sp)+,r1
	jmp	*reltab(r0)
_REL4:
	cvtbl	(r10)+,r0
	ashl	$1,r0,r0	#maintain compatability
	movl	(sp)+,r1
	cmpl	(sp)+,r1
	jmp	*reltab(r0)
_REL28:
	cvtbl	(r10)+,r0
	ashl	$1,r0,r0	#maintain compatability
	cvtwd	(sp)+,r1
	cmpd	(sp)+,r1
	jmp	*reltab(r0)
_REL48:
	cvtbl	(r10)+,r0
	ashl	$1,r0,r0	#maintain compatability
	cvtld	(sp)+,r1
	cmpd	(sp)+,r1
	jmp	*reltab(r0)
_REL82:
	cvtbl	(r10)+,r0
	ashl	$1,r0,r0	#maintain compatability
	movd	(sp)+,r1
	cvtwd	(sp)+,r3
	cmpd	r3,r1
	jmp	*reltab(r0)
_REL84:
	cvtbl	(r10)+,r0
	ashl	$1,r0,r0	#maintain compatability
	movd	(sp)+,r1
	cvtld	(sp)+,r3
	cmpd	r3,r1
	jmp	*reltab(r0)
_REL8:
	cvtbl	(r10)+,r0
	ashl	$1,r0,r0	#maintain compatability
	movd	(sp)+,r1
	cmpd	(sp)+,r1
	jmp	*reltab(r0)
_RELG:
	cvtbl	(r10)+,r5	#r5 has jump opcode
	ashl	$1,r5,r5	#maintain compatability
	cvtwl	(r10)+,r1	#r1 has comparison length
	movl	r1,r4		#r4 has stack length
	blbc	r4,l0201
	incl	r4
l0201:	addl3	sp,r4,r3	#r3 has addr of bottom operand
	addl2	r3,r4		#r4 points to cleared stack
	cmpc3	r1,(r3),(sp)	#do comparison
	movpsl	r2		#save condition codes
	movl	r4,sp		#update stack
	bicpsw	$15		#restore condition codes
	bispsw	r2
	jmp	*reltab(r5)

	.align	1
reltab:
	.long	releq
	.long	relne
	.long	rellt
	.long	relgt
	.long	relle
	.long	relge
	.long	ifeq
	.long	ifne
	.long	iflt
	.long	ifgt
	.long	ifle
	.long	ifge

releq:
	beql	True
	clrw	-(sp)
	jmp	(r8)
relne:
	bneq	True
	clrw	-(sp)
	jmp	(r8)
rellt:
	blss	True
	clrw	-(sp)
	jmp	(r8)
relgt:
	bgtr	True
	clrw	-(sp)
	jmp	(r8)
relle:
	bleq	True
	clrw	-(sp)
	jmp	(r8)
relge:
	bgeq	True
	clrw	-(sp)
	jmp	(r8)
True:
	movw	$1,-(sp)
	jmp	(r8)
ifeq:
	bneq	iftra
	addl2	$2,r10
	jmp	(r8)
ifne:
	beql	iftra
	addl2	$2,r10
	jmp	(r8)
iflt:
	bgeq	iftra
	addl2	$2,r10
	jmp	(r8)
ifgt:
	bleq	iftra
	addl2	$2,r10
	jmp	(r8)
ifle:
	bgtr	iftra
	addl2	$2,r10
	jmp	(r8)
ifge:
	blss	iftra
	addl2	$2,r10
	jmp	(r8)
iftra:
	cvtwl	(r10),r0
	addl2	r0,r10
	jmp	(r8)
