#
# 24pxp.s
#
# PXP stuff
#
_PXPBUF:
	incl	r10
	cvtwl	(r10),r0
	addl2	$3,r0
	ashl	$2,r0,r0
	movl	r0,_pxpsize
	pushl	r0
	calls	$1,_palloc
	movl	r0,_pxpbuf
	movl	r0,r6
	cvtwl	$0426,(r6)+
	calls	$0,_time
	movl	r0,(r6)+
	cvtlw	(r10)+,(r6)+
	movw	(r10)+,(r6)+
	jmp	(r8)
_TRACNT:
	incl	r10
	cvtwl	6(r10),r0
	addl3	(r10),ap,r10
	incl	*_pxpbuf[r0]
	jmp	(r8)
_COUNT:
	incl	r10
	cvtwl	(r10)+,r0
	incl	*_pxpbuf[r0]
	jmp	(r8)
