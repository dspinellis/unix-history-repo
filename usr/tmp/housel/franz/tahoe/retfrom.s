# this file is here because some necessary register moves were
# being optimized away.
	.text
LL0:	.align	1
	.globl	_Iretfromfr
	.set	L136,0x1000
	.text
_Iretfromfr:.word	L136
	movl	4(fp),r12
	pushl	8(r12)
	callf	$8,_xpopnames
	movl	r12,r0
	subl3	$24,r12,sp
	movl	(sp),r8
	movl	4(sp),r9
	movl	8(sp),r10
	movl	12(sp),r11
	movl	16(sp),r12
	movl	20(sp),r13
	movl	24(sp),r7
	movl	28(sp),r6
	jmp	*40(sp)
	
