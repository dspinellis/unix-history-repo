#
# 17rv.s
#
# LVALUES and RVALUES
#
_LV:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl3	_display[r0],r1,-(sp)
	jmp	(r8)
_RV1:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl2	_display[r0],r1
	cvtbw	(r1),-(sp)
	jmp	(r8)
_RV2:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl2	_display[r0],r1
	movw	(r1),-(sp)
	jmp	(r8)
_RV4:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl2	_display[r0],r1
	pushl	(r1)
	jmp	(r8)
_RV8:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl2	_display[r0],r1
	movq	(r1),-(sp)
	jmp	(r8)
_RV:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl2	_display[r0],r1	#r1 points to string o be moved
	cvtwl	(r10)+,r0	#r0 has length of string to be moved
movblk:
	movl	r0,r2		#r2 has length of stack space
	blbc	r2,l1701	#adjust r2 to word boundry
	incl	r2
l1701:
	subl2	r2,sp		#allocate stack space
	movc5	r0,(r1),$0,r2,(sp)   #move string to stack
	jmp	(r8)
