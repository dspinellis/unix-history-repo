#
# 07sub.s
#
# SUBTRACTION
#
_SUB2:
	incl	r10
	cvtwl	(sp)+,r0
	cvtwl	(sp)+,r1
	subl3	r0,r1,-(sp)
	jmp	(r8)
_SUB24:
	incl	r10
	cvtwl	(sp)+,r0
	subl2	r0,(sp)
	jmp	(r8)
_SUB42:
	incl	r10
	movl	(sp)+,r0
	cvtwl	(sp)+,r1
	subl3	r0,r1,-(sp)
	jmp	(r8)
_SUB4:
	incl	r10
	subl2	(sp)+,(sp)
	jmp	(r8)
_SUB28:
	incl	r10
	cvtwd	(sp)+,r0
	subd2	r0,(sp)
	jmp	(r8)
_SUB82:
	incl	r10
	movd	(sp)+,r0
	cvtwd	(sp)+,r2
	subd3	r0,r2,-(sp)
	jmp	(r8)
_SUB48:
	incl	r10
	cvtld	(sp)+,r0
	subd2	r0,(sp)
	jmp	(r8)
_SUB84:
	incl	r10
	movd	(sp)+,r0
	cvtld	(sp)+,r2
	subd3	r0,r2,-(sp)
	jmp	(r8)
_SUB8:
	incl	r10
	subd2	(sp)+,(sp)
	jmp	(r8)
