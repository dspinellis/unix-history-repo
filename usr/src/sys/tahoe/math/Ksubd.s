	.data
	.text
LL0:	.align	1
	.globl	_Ksubd
	.set	L12,0x0
	.data
	.text
_Ksubd:	.word	L12	# _Ksubd(acc_most,acc_least,op_most,op_least,hfs)
	tstl	4(fp)
	jneq	next
	movl	16(fp),r1
	movl	12(fp),r0
	lnd	r0
	std	r0
	ret
next:
	tstl	12(fp)
	jneq	doit
	movl	8(fp),r1
	movl	4(fp),r0
	ret
doit:
	lnd	12(fp)		# -op
	pushl	20(fp)		# hfs
	pushd			# push op_least op_most
	pushl	8(fp)
	pushl	4(fp)		# acc
	callf	$24,_Kaddd
	ret


/*
 * double 
 * subd(d1,d2)
 * double d1,d2;
 * {
 * 	return(d1+(-d2));
 * }
*/
