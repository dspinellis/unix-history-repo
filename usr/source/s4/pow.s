/ C library -- pow (power)

.globl	_pow
.globl	savr5
.globl	retrn
.globl	pow

_pow:
	mov	r5,-(sp)
	mov	sp,r5
	mov	r5,savr5
	movf	4(r5),fr0
	movf	12.(r5),fr1
	jsr	r5,pow
	clr	savr5
	jmp	retrn

