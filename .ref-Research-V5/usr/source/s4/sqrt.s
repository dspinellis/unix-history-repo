.globl	_sqrt,sqrt,retrn,savr5
_sqrt:
	mov	r5,-(sp)
	mov	sp,r5
	mov	r5,savr5
	movf	4(r5),fr0
	jsr	r5,sqrt
	clr	savr5
	jmp	retrn
