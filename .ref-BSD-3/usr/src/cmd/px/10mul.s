#
# 10mul.s
#
# MULTIPLICATION AND SQUARING
#
_SQR2:
	movw	(sp),-(sp)
_MUL2:
	incl	r10
	cvtwl	(sp)+,r0
	cvtwl	(sp)+,r1
	mull3	r0,r1,-(sp)
	jmp	(r8)
_MUL24:
	incl	r10
	cvtwl	(sp)+,r0
	mull2	r0,(sp)
	jmp	(r8)
_MUL42:
	incl	r10
	movl	(sp)+,r0
	cvtwl	(sp)+,r1
	mull3	r0,r1,-(sp)
	jmp	(r8)
_SQR4:
	movl	(sp),-(sp)
_MUL4:
	incl	r10
	mull2	(sp)+,(sp)
	jmp	(r8)
_MUL28:
	incl	r10
	cvtwd	(sp)+,r0
	muld2	r0,(sp)
	jmp	(r8)
_MUL82:
	incl	r10
	movd	(sp)+,r0
	cvtwd	(sp)+,r2
	muld3	r0,r2,-(sp)
	jmp	(r8)
_MUL48:
	incl	r10
	cvtld	(sp)+,r0
	muld2	r0,(sp)
	jmp	(r8)
_MUL84:
	incl	r10
	movd	(sp)+,r0
	cvtld	(sp)+,r2
	muld3	r0,r2,-(sp)
	jmp	(r8)
_SQR8:
	movd	(sp),-(sp)
_MUL8:
	incl	r10
	muld2	(sp)+,(sp)
	jmp	(r8)
