#
# 02relset.s
#
# RELATIONALS ON SETS
#
_RELT:
	cvtbl	(r10)+,r5	#r5 has jump opcode
	cvtwl	(r10)+,r1	#r1 has comparison length (bytes)
	movl	r1,r4		#r4 has stack length
	blbc	r4,l0211
	incl	r4
l0211:
	addl3	sp,r4,r3	#r3 has addr of lower operand
	addl2	r3,r4		#r4 points to cleared stack
	ashl	$1,r5,r5	#maintain compatability
	jsb	*settab(r5)	#calc condition, return boolean in r0
	movl	r4,sp		#reset stack
	cmpl	r5,$20		#check for branch
	bgtr	l0212
	movw	r0,-(sp)	#put boolean on stack
	jmp	(r8)
l0212:
	tstl	r0
	beql	l0213
	addl2	$2,r10		#continue on true
	jmp	(r8)
l0213:
	cvtwl	(r10),r0	#skip on false
	addl2	r0,r10
	jmp	(r8)

	.align	1
settab:			#condition code branch table
	.long	seteq	#generate boolean answer
	.long	setne
	.long	setlt
	.long	setgt
	.long	setle
	.long	setge
	.long	seteq	#branch on condition false
	.long	setne
	.long	setlt
	.long	setgt
	.long	setle
	.long	setge

setlt:
	moval	4(sp),r2	#skip over return address
	ashl	$-1,r1,r1
l0214:
	bicw3	(r2),(r3),r0
	bneq	false
	bicw2	(r3)+,(r2)+
	bneq	l0216		#need only check <= for remainder of set
	sobgtr	r1,l0214
false:
	clrl	r0
	rsb
setle:
	moval	4(sp),r2
	ashl	$-1,r1,r1
l0215:
	bicw2	(r2)+,(r3)+
	bneq	false
l0216:
	sobgtr	r1,l0215
true:
	movl	$1,r0
	rsb
seteq:
	cmpc3	r1,(r3),4(sp)
	beql	true
	clrl	r0
	rsb
setne:
	cmpc3	r1,(r3),4(sp)
	bneq	true
	clrl	r0
	rsb
setgt:
	moval	4(sp),r2
	ashl	$-1,r1,r1
l0217:
	bicw3	(r3),(r2),r0
	bneq	false
	bicw2	(r2)+,(r3)+
	bneq	l0219		#need only check >= for remainder of set
	sobgtr	r1,l0217
	clrl	r0
	rsb
setge:
	moval	4(sp),r2
	ashl	$-1,r1,r1
l0218:
	bicw2	(r3)+,(r2)+
	bneq	false
l0219:
	sobgtr	r1,l0218
	movl	$1,r0
	rsb
