#
# 21rang.s
#
# range checking
#
_RANG2:
	cvtbl	(r10)+,r1
	bneq	l2101
	cvtwl	(r10)+,r1
l2101:
	cvtwl	(r10)+,r2
	cvtwl	(sp),r0
	index	r0,r1,r2,$1,$1,r3
	jmp	(r8)
_RANG24:
	incl	r10
	cvtwl	(sp),r0
	index	r0,(r10)+,(r10)+,$1,$1,r2
	jmp	(r8)
_RANG42:
	cvtbl	(r10)+,r0
	bneq	l2102
	cvtwl	(r10)+,r0
l2102:
	cvtwl	(r10)+,r1
	index	(sp),r0,r1,$1,$1,r2
	jmp	(r8)
_RANG4:
	incl	r10
	index	(sp),(r10)+,(r10)+,$1,$1,r2
	jmp	(r8)
_RSNG2:
	cvtbl	(r10)+,r1
	bneq	l2103
	cvtwl	(r10)+,r1
l2103:
	cvtwl	(sp),r0
	index	r0,$0,r1,$1,$1,r2
	jmp	(r8)
_RSNG24:
	incl	r10
	cvtwl	(sp),r0
	index	r0,$0,(r10)+,$1,$1,r2
	jmp	(r8)
_RSNG42:
	cvtbl	(r10)+,r1
	bneq	l2104
	cvtwl	(r10)+,r1
l2104:
	index	(sp),$0,r1,$1,$1,r2
	jmp	(r8)
_RSNG4:
	incl	r10
	index	(sp),$0,(r10)+,$1,$1,r2
	jmp	(r8)
