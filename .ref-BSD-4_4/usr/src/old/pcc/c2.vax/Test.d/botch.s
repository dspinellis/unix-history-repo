LL0:
	.data
	.text
	.align	1
	.globl	_main
_main:
	.word	L12
	jbr 	L14
L15:
	addl3	$1,r11,r0
	divl3	$8,r0,r1
	mull2	$8,r1
	subl3	r1,r0,r1
	tstl	r1
	jeql	L9999
	movl	$10,r0
	jbr	L9998
L9999:
	movl	$20,r0
L9998:
	movl	r0,r10
	ret
	.set	L12,0xc00
L14:
	jbr 	L15
	.data
