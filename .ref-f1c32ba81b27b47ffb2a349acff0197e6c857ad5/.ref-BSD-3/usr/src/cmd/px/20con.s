#
# 20con.s
#
# CONOPS
#
_CON1:
	cvtbw	(r10)+,-(sp)
	jmp	(r8)
_CON2:
	incl	r10
	movw	(r10)+,-(sp)
	jmp	(r8)
_CON4:
	incl	r10
	pushl	(r10)+
	jmp	(r8)
_CON8:
	incl	r10
	movd	(r10)+,-(sp)
	jmp	(r8)
_CON:
	cvtbl	(r10)+,r0
	bneq	l2001
	cvtwl	(r10)+,r0	#r0 has length to be moved
l2001:
	movl	r10,r1		#r1 has addr of data to be moved
	movl	r0,r2		#r2 has length of stack space
	blbc	r2,l2002
	incl	r2
l2002:
	subl2	r2,sp		#allocate stack space
	addl2	r2,r10		#advance over data
	movc5	r0,(r1),$0,r2,(sp)   #move string to stack
	jmp	(r8)
