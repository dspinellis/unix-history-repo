#
# 12div.s
#
# INTEGER DIVISION
#
_DIV2:
	incl	r10
	cvtwl	(sp)+,r0
	cvtwl	(sp)+,r1
	divl3	r0,r1,-(sp)
	jmp	(r8)
_DIV24:
	incl	r10
	cvtwl	(sp)+,r0
	divl2	r0,(sp)
	jmp	(r8)
_DIV42:
	incl	r10
	movl	(sp)+,r0
	cvtwl	(sp)+,r1
	divl3	r0,r1,-(sp)
	jmp	(r8)
_DIV4:
	incl	r10
	divl2	(sp)+,(sp)
	jmp	(r8)
