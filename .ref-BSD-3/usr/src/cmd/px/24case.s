#
# 24case.s
#
# CASE OPERATORS
#
_CASE1OP:
	cvtbw	(r10)+,r0
	bneq	l2401
	cvtwl	(r10)+,r0	#r0 has length of case table
l2401:
	movaw	(r10)[r0],r2	#r2 has pointer to cases
	cvtwb	(sp)+,r3	#r3 has case element to find
	locc	r3,r0,(r2)	#find case element
	beql	caserr		#case not found
offset:
	mnegl	r0,r0		#calc new lc
	cvtwl	(r2)[r0],r1
	addl2	r1,r10
	jmp	(r8)
_CASE2OP:
	cvtbl	(r10)+,r0
	bneq	l2402
	cvtwl	(r10)+,r0	#r0 has length of case table
l2402:
	movaw	(r10)[r0],r1	#r1 has pointer to cases
	movl	r1,r2		#save base pointer
	movw	(sp)+,r3	#r3 has case element to find
l2403:
	cmpw	r3,(r1)+	#search for case
	beqlu	offset
	sobgtr	r0,l2403
	brb	caserr		#not found
_CASE4OP:
	cvtbl	(r10)+,r0
	bneq	l2404
	cvtwl	(r10)+,r0	#r0 has length of case table
l2404:
	movaw	(r10)[r0],r1	#r1 has pointer to cases
	movl	r1,r2		#save base pointer
	movl	(sp)+,r3	#r3 has case element to find
l2405:
	cmpl	r3,(r1)+	#search for case
	beqlu	offset
	sobgtr	r0,l2405
caserr:
	movw	$ECASE,_perrno
	jbr	error
