#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)04as.s 4.1 10/10/80";
#
# ASSIGNMENT OPERATORS
#
_AS2:
	incl	r10
	movw	(sp)+,*(sp)+
	jmp	(r8)
_AS24:
	incl	r10
	cvtwl	(sp)+,*(sp)+
	jmp	(r8)
_AS42:
	incl	r10
	cvtlw	(sp)+,*(sp)+
	jmp	(r8)
_AS4:
	incl	r10
	movl	(sp)+,*(sp)+
	jmp	(r8)
_AS21:
	incl	r10
	cvtwb	(sp)+,*(sp)+
	jmp	(r8)
_AS41:
	incl	r10
	cvtlb	(sp)+,*(sp)+
	jmp	(r8)
_AS28:
	incl	r10
	cvtwd	(sp)+,*(sp)+
	jmp	(r8)
_AS48:
	incl	r10
	cvtld	(sp)+,*(sp)+
	jmp	(r8)
_AS8:
	incl	r10
	movd	(sp)+,*(sp)+
	jmp	(r8)
_AS:
	cvtbl	(r10)+,r0
	bneq	l0401
	movzwl	(r10)+,r0	#r0 has data length in bytes
l0401:
	addl3	sp,r0,r6	#r6 points to destination addr
	blbc	r6,l0402	#adjust for word boundry
	incl	r6
l0402:
	movc3	r0,(sp),*(r6)+	#move data from stack to dest
	movl	r6,sp		#update stack pointer
	jmp	(r8)
