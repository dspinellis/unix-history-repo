	.data
	.comm	__dbargs,512
	.text
	.align	1
	.globl	__dbsubc
__dbsubc:
	callg	__dbargs+4,*__dbargs
	.globl	__dbsubn
__dbsubn:
	halt

	.data
