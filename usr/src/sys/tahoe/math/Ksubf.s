	.data
	.text
LL0:	.align	1
	.globl	_Ksubf
	.set	L12,0xffc
	.data
	.text
_Ksubf:	.word	L12	# _Ksubf(acc_most,acc_least,op_most,op_least,hfs)
	tstl	4(fp)	# handle (a-b) where a and/or b = 0.0
	jneq	next
	movl	$0,r1
	movl	12(fp),r0
	lnd	r0
	std	r0
	ret
next:
	tstl	12(fp)
	jneq	doit
	movl	$0,r1
	movl	4(fp),r0
	ret
doit:
	lnd	12(fp)	# note: -(0) = 0 by definition!
	pushl	20(fp)	# hfs
	pushd	
	pushl	8(fp)
	pushl	4(fp)	# acc
	callf	$24,_Kaddf
	ret


/*
 * subf(d1,d2)
 * {
 * 	return(d1+(-d2));
 * }
*/
