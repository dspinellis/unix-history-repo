#
# 06add.s
#
# ADDITION
#
_ADD2:
	incl	r10
	cvtwl	(sp)+,r0
	cvtwl	(sp)+,r1
	addl3	r0,r1,-(sp)
	jmp	(r8)
_ADD24:
	incl	r10
	cvtwl	(sp)+,r0
	addl2	r0,(sp)
	jmp	(r8)
_ADD42:
	incl	r10
	movl	(sp)+,r0
	cvtwl	(sp)+,r1
	addl3	r0,r1,-(sp)
	jmp	(r8)
_ADD4:
	incl	r10
	addl2	(sp)+,(sp)
	jmp	(r8)
_ADD28:
	incl	r10
	cvtwd	(sp)+,r0
	addd2	r0,(sp)
	jmp	(r8)
_ADD82:
	incl	r10
	movd	(sp)+,r0
	cvtwd	(sp)+,r2
	addd3	r0,r2,-(sp)
	jmp	(r8)
_ADD48:
	incl	r10
	cvtld	(sp)+,r0
	addd2	r0,(sp)
	jmp	(r8)
_ADD84:
	incl	r10
	movd	(sp)+,r0
	cvtld	(sp)+,r2
	addd3	r0,r2,-(sp)
	jmp	(r8)
_ADD8:
	incl	r10
	addd2	(sp)+,(sp)
	jmp	(r8)
