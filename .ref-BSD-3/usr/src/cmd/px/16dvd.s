#
# 16dvd.s
#
# FLOATING DIVISION
#
_DVD2:
	incl	r10
	cvtwd	(sp)+,r0
	cvtwd	(sp)+,r2
	divd3	r0,r2,-(sp)
	jmp	(r8)
_DVD24:
	incl	r10
	cvtwd	(sp)+,r0
	cvtld	(sp)+,r2
	divd3	r0,r2,-(sp)
	jmp	(r8)
_DVD42:
	incl	r10
	cvtld	(sp)+,r0
	cvtwd	(sp)+,r2
	divd3	r0,r2,-(sp)
	jmp	(r8)
_DVD4:
	incl	r10
	cvtld	(sp),r0
	cvtld	4(sp),r2
	divd3	r0,r2,(sp)
	jmp	(r8)
_DVD28:
	incl	r10
	cvtwd	(sp)+,r0
	divd2	r0,(sp)
	jmp	(r8)
_DVD82:
	incl	r10
	movd	(sp)+,r0
	cvtwd	(sp)+,r2
	divd3	r0,r2,-(sp)
	jmp	(r8)
_DVD48:
	incl	r10
	cvtld	(sp)+,r0
	divd2	r0,(sp)
	jmp	(r8)
_DVD84:
	incl	r10
	movd	(sp)+,r0
	cvtld	(sp)+,r2
	divd3	r0,r2,-(sp)
	jmp	(r8)
_DVD8:
	incl	r10
	divd2	(sp)+,(sp)
	jmp	(r8)
