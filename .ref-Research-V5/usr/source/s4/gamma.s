.globl	_gamma,gamma,retrn,savr5
_gamma:
	mov	r5,-(sp)
	mov	sp,r5
	mov	r5,savr5
	movf	4(r5),fr0
	jsr	r5,gamma
	clr	savr5
	jmp	retrn
