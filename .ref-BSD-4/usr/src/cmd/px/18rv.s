#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)18rv.s 4.1 10/10/80";
#
# LVALUES and RVALUES
#
_LV:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl3	_display(r0),r1,-(sp)
	jmp	(r8)
_LLV:
	cvtbl	(r10)+,r0
	addl3	_display(r0),(r10)+,-(sp)
	jmp	(r8)
_RV1:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl2	_display(r0),r1
	cvtbw	(r1),-(sp)
	jmp	(r8)
_LRV1:
	cvtbl	(r10)+,r0
	addl3	_display(r0),(r10)+,r1
	cvtbw	(r1),-(sp)
	jmp	(r8)
_RV14:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl2	_display(r0),r1
	cvtbl	(r1),-(sp)
	jmp	(r8)
_LRV14:
	cvtbl	(r10)+,r0
	addl3	_display(r0),(r10)+,r1
	cvtbl	(r1),-(sp)
	jmp	(r8)
_RV2:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl2	_display(r0),r1
	movw	(r1),-(sp)
	jmp	(r8)
_LRV2:
	cvtbl	(r10)+,r0
	addl3	_display(r0),(r10)+,r1
	movw	(r1),-(sp)
	jmp	(r8)
_RV24:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl2	_display(r0),r1
	cvtwl	(r1),-(sp)
	jmp	(r8)
_LRV24:
	cvtbl	(r10)+,r0
	addl3	_display(r0),(r10)+,r1
	cvtwl	(r1),-(sp)
	jmp	(r8)
_RV4:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl2	_display(r0),r1
	pushl	(r1)
	jmp	(r8)
_LRV4:
	cvtbl	(r10)+,r0
	addl3	_display(r0),(r10)+,r1
	pushl	(r1)
	jmp	(r8)
_RV8:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl2	_display(r0),r1
	movq	(r1),-(sp)
	jmp	(r8)
_LRV8:
	cvtbl	(r10)+,r0
	addl3	_display(r0),(r10)+,r1
	movq	(r1),-(sp)
	jmp	(r8)
_RV:
	cvtbl	(r10)+,r0
	cvtwl	(r10)+,r1
	addl2	_display(r0),r1	#r1 points to string o be moved
	movzwl	(r10)+,r0	#r0 has length of string to be moved
	movl	r0,r2		#r2 has length of stack space
	blbc	r2,l1801	#adjust r2 to word boundry
	incl	r2
l1801:
	subl2	r2,sp		#allocate stack space
	movc5	r0,(r1),$0,r2,(sp)   #move string to stack
	jmp	(r8)
_LRV:
	cvtbl	(r10)+,r0
	addl3	_display(r0),(r10)+,r1   #r1 points to string to be moved
	movzwl	(r10)+,r0	#r0 has length of string to be moved
	movl	r0,r2		#r2 has length of stack space
	blbc	r2,l1802	#adjust r2 to word boundry
	incl	r2
l1802:
	subl2	r2,sp		#allocate stack space
	movc5	r0,(r1),$0,r2,(sp)   #move string to stack
	jmp	(r8)
