	.globl	_inewint
_inewint:.word	0
	cvtlb	4(ap),r0
	bvs	nofit
	ashl	$2,4(ap),r0
	addl2	$1024,r0
	ret
nofit:
	calls	$0,_newint
	movl	4(ap),0(r0)
	ret
