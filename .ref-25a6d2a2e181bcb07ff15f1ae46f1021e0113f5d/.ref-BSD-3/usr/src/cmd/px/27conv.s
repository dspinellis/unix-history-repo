#
# 27conv.s
#
# CONVERSIONS
#
_STOI:
	incl	r10
	cvtwl	(sp)+,-(sp)
	jmp	(r8)
_STOD:
	incl	r10
	cvtwd	(sp)+,-(sp)
	jmp	(r8)
_ITOD:
	incl	r10
	cvtld	(sp)+,-(sp)
	jmp	(r8)
_ITOS:
	incl	r10
	cvtlw	(sp)+,-(sp)
	jmp	(r8)
