#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)17ind.s 4.1 10/10/80";
#
# INDS
#
_IND1:
	incl	r10
	cvtbw	*(sp)+,-(sp)
	jmp	(r8)
_IND14:
	incl	r10
	cvtbl	*(sp)+,-(sp)
	jmp	(r8)
_IND2:
	incl	r10
	movw	*(sp)+,-(sp)
	jmp	(r8)
_IND24:
	incl	r10
	cvtwl	*(sp)+,-(sp)
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
	jneq	l1701
	movzwl	(r10)+,r0
l1701:
	movl	r0,r2		#r2 has length of stack space
	blbc	r2,l1702	#adjust r2 to word boundry
	incl	r2
l1702:
	subl2	r2,sp		#allocate stack space
	movc5	r0,(r1),$0,r2,(sp)   #move string to stack
	jmp	(r8)
