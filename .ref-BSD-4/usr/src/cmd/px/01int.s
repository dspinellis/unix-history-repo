#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)01int.s 4.1 10/10/80";
#
_HALT:
	movw	$EHALT,_perrno
	jbr	error

_GOTO:
	cvtbl	(r10)+,r0
	movl	_display(r0),r6	#r6 has exit dp value
	addl3	(r10),ap,r10	#establish return address
L0101:
	cmpl	r6,(r9)		#check for done
	blss	egoto		#missed the requested frame
	beql	L0102
	pushl	(r9)		#flush and close local files
	calls	$1,_pclose
	movl	(r9),sp		#deallocate local vars
	movl	16(sp),(r9)	#restore old display entry
	movl	20(sp),r9	#get old display pointer
	brb	L0101		#continue
L0102:
	movl	4(sp),_file	#restore old I/O info
	movl	8(sp),r7
	movl	*(r9),sp	#reset sp to top of stack
	jmp	(r8)
egoto:
	movl	$EGOTO,_perrno
	jbr	error

_LINO:
	cmpl	*(r9),sp	#check stack integrity
	jneq	stknemp
	cvtbl	(r10)+,r11	#update line number
	bneq	L0103
	movzwl	(r10)+,r11
L0103:
	aoblss	_stlim,_stcnt,L0104   #update statement count
	movw	$ESTLIM,_perrno
	jbr	error
stknemp:
	movw	$ESTKNEMP,_perrno
	jbr	error
L0104:
	jmp	(r8)

_IF:
	tstw	(sp)+
	beql	_TRA
	addl2	$3,r10
	jmp	(r8)

_TRA4:
	addl3	1(r10),ap,r10
	jmp	(r8)

_TRA:
	incl	r10
	cvtwl	(r10),r0
	addl2	r0,r10
	jmp	(r8)

_PUSH:
	cvtbl	(r10)+,r0
	bneq	L0105
	movl	(r10)+,r0
L0105:
	mnegl	r0,r0
	blbc	r0,L0106
	incl	r0
L0106:
	subl3	r0,sp,r1	#r1 points to new top of stack
	clrl	tempsize(r1)	#check for memory (fault => not available)
	movl	r1,sp		#allocate the memory
	movc5	$0,(r2),$0,r0,(sp)
	jmp	(r8)

_SDUP2:
	incl	r10
	movw	(sp),-(sp)
	jmp	(r8)

_SDUP4:
	incl	r10
	movl	(sp),-(sp)
	jmp	(r8)

_ASRT:
	incl	r10
	tstw	(sp)+
	beql	L0107
	jmp	(r8)
L0107:
	movw	$EASRT,_perrno
	jbr	error
