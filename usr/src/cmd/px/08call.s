#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)08call.s 4.1 10/10/80";
#
_CALL:
	cvtbl	(r10)+,r0	#entry level of new proc
	movl	(r10)+,r1	#new entry point
	pushr	$R11|R10|R9	#save lino, lc, dp
	addl2	ap,r1		#calc new entry point
	addl3	2(r1),ap,r10
	moval	_display(r0),r9	#set up new display pointer
	jmp	(r8)

_FCALL:
	cvtbl	(r10)+,r0	#r0 has number of bytes of arguments
	bneq	L0801
	movl	(r10)+,r0
L0801:
	movl	(sp)+,r6	#r6 points to formal call struct
	pushr	$R11|R10|R9	#save lino, lc, dp
	movl	(r6),r10	#set new entry point
	cmpl	6(r10),r0	#check arg count
	bneq	enargs
	addl3	$_display,4(r6),r9	#set up new display pointer
	movl	4(r6),r1	#save current display, restore formal display
	movc3	r1,_display+4,8(r6)[r1]
	movc3	4(r6),8(r6),_display+4
	jmp	(r8)
enargs:
	movw	$ENARGS,_perrno
	jbr	error

_FSAV:
	movl	(sp),r6		#r6 points to formal call struct
	cvtbl	(r10)+,4(r6)	#set block number
	addl3	(r10)+,ap,r0	#r0 pts to TRA4
	addl3	2(r0),ap,(r6)	#set entry address
	movc3	4(r6),_display+4,8(r6)	#save current display
	jmp	(r8)

_FRTN:
	cvtbl	(r10)+,r0	#r0 has size of returned object
	bneq	L0802
	movzwl	(r10)+,r0
L0802:
	addl3	r0,sp,r1	#r1 points to stack loc of formal call struct
	movl	(r1),r6		#r6 points to formal call struct
	movc3	r0,(sp),4(sp)	#move down the returned value
	addl2	$4,sp		#throw away leftover
	movl	4(r6),r1	#r1 has display size
	movc3	r1,8(r6)[r1],_display+4	#restore previous display
	jmp	(r8)
#
_NODUMP:
	moval	iloop,r8	#disable profiling
	incw	_nodump
	bicpsw	$0xe0		#disable overflow checks

_BEG:
	movzbl	(r10)+,r1	#r1 has name size
	movl	(r10)+,r0	#r0 has local variable size
	addl2	$4,r9		#enter local scope
	pushl	(r9)		#save old display value
	pushal	(r10)+		#pointer to entry info
	movzwl	(r10)+,r11	#set new lino
	addl2	r1,r10		#skip over name text
	pushl	r7		#save I/O info
	pushl	_file
	subl2	$4,sp		#space for top of frame pointer
	movl	sp,(r9)		#set new display pointer
	addl3	r0,sp,r3	#r3 points to new top of stack
	clrl	tempsize(r3)	#check for memory (fault => not available)
	movl	r3,(sp)		#set new top of stack pointer
	movl	r3,sp		#allocate local variables
	mnegl	r0,r6		#r6 has amount of space to alloc
	cmpl	r6,$65535	#check for out of character range
	bleq	L0804
L0803:
	movc5	$0,(r2),$0,$65535,(r3)	#continue zero of local variables
	acbl	$65536,$-65535,r6,L0803	#deduct amount zeroed and continue
L0804:
	movc5	$0,(r2),$0,r6,(r3)	#zero out local variables
	jmp	(r8)

_END:
	pushl	(r9)		#flush and close local files
	calls	$1,_pclose
	movl	(r9),sp		#deallocate local vars
	addl2	$4,sp		#pop TOS ptr
	movl	(sp)+,_file	#restore old I/O info
	movl	(sp)+,r7
	movzwl	*(sp)+,r0	#r0 has number of bytes of parameters
	movl	(sp)+,(r9)	#restore old display entry
	cmpl	r9,$_display+4	#exiting main proc ???
	beql	L0805
	popr	$R11|R10|R9	#restore lino, lc, dp
	addl2	r0,sp		#deallocate parameters
	jmp	(r8)
L0805:
	ret			#end of interpretation
