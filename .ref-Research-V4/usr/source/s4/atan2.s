.globl	_atan2,atan2,retrn,savr5
_atan2:
	mov	r5,-(sp)
	mov	sp,r5
	mov	r5,savr5
	movf	4(r5),fr0
	movf	12.(r5),fr1
	jsr	r5,atan2
	clr	savr5
	jmp	retrn
