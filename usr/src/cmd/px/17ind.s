#
# 17ind.s
#
# INDS
#
_IND1:
	incl	r10
	cvtbw	*(sp)+,-(sp)
	jmp	(r8)
_IND2:
	incl	r10
	movw	*(sp)+,-(sp)
	jmp	(r8)
_IND4:
	incl	r10
	pushl	*(sp)+
	jmp	(r8)
_IND8:
	incl	r10
	movq	*(sp)+,-(sp)
	jmp	(r8)
_IND:
	movl	(sp)+,r1
	cvtbl	(r10)+,r0
	jneq	movblk
	cvtwl	(r10)+,r0
	jbr	movblk
