/ C library -- intr

/ intr(0); /* exit on interrupt */
/ intr(anything_odd); /* ignore interrupts */
/ intr(label); /* goto label on interrupts */

	.globl	_intr

.data
_intr:
	1f
.text
1:
	mov	2(sp),r0
	beq	1f
	bit	$1,r0
	beq	2f
1:
	bic	$1,r0
	mov	r0,0f
	sys	intr; 0:..
	rts	pc
2:
	mov	r5,9f
	mov	r0,9f+2
	sys	intr; 1f
	rts	pc

/ here on interrupts

1:
	mov	9f,r5
	jmp	*9f+2

.bss
9:
	.=.+4

