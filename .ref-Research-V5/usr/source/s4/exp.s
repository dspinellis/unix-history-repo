.globl	_exp,exp,retrn,savr5
_exp:
	mov	r5,-(sp)
	mov	sp,r5
	mov	r5,savr5
	movf	4(r5),fr0
	jsr	r5,exp
	clr	savr5
	jmp	retrn
