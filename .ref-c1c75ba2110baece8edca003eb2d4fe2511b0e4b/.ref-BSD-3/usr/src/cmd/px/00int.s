#
# 00int.s
#
_ABORT:
	halt			#stop immediately!!!
_HALT:
	movw	$EHALT,_perrno
	jbr	error
_CALL:
	cvtbl	(r10)+,r0	#entry level of new proc
	movl	(r10)+,r1	#new entry point
	pushr	$R11|R10|R9	#save lino, lc, dp
	moval	_display[r0],r9
	addl3	r1,ap,r10	#calc new entry point
	jmp	(r8)
_NODUMP:
	moval	iloop,r8	#disable profiling
	incw	_nodump
	bicpsw	$0xe0		#disable overflow checks
_BEG:
	incl	r10
	cvtwl	(r10)+,r0	#r0 has local variable size
	bgtr	framesize	#current max is 32768 bytes
	addl2	_addrsze,r9	#enter local scope
	pushl	(r9)		#save old display value
	pushl	r10		#pointer to proc name
	addl2	$8,r10		#skip over name text
	movzwl	(r10)+,r11	#set new lino
	pushl	r7		#save I/O info
	pushl	_file
	subl2	$4,sp		#space for top of frame pointer
	movl	sp,(r9)		#set new display pointer
	addl3	r0,sp,r1	#r1 points to new top of stack
	clrl	tempsize(r1)	#check for memory (fault => not available)
	movl	r1,(sp)		#set new top of stack pointer
	movl	r1,sp		#allocate local variables
	mnegl	r0,r0		#r0 has amount of space to alloc
	movc5	$0,(r2),$0,r0,(sp)   #zero out local variables
	jmp	(r8)
framesize:
	movw	$EFRAMESIZE,_perrno
	jbr	error
_BLKBEG:
	incl	r10
	mnegl	(sp)+,r0	#r0 has size of block
	cmpl	*(r9),sp	#check stack integrity
	bneq	stknemp
	subl3	r0,sp,r1	#r1 points to new top of stack
	clrl	tempsize(r1)	#check for memory (fault => not available)
	movl	r1,sp		#allocate space
	movl	sp,*(dp)	#set top of stack ptr
	movc5	$0,(r2),$0,r0,(sp)   #clear space
	jmp	(r8)
stknemp:
	movw	$ESTKNEMP,_perrno
	jbr	error
_END:
	pushl	(r9)		#flush and close local files
	calls	$1,_pclose
	movl	(r9),sp		#deallocate local vars
	movl	4(sp),_file	#restore old I/O info
	movl	8(sp),r7
	movl	16(sp),(r9)	#restore old display entry
	addl2	$20,sp		#deallocate current frame
	addl3	$_display,_addrsze,r0
	cmpl	r9,r0		#exiting main proc ???
	beql	l0011
	popr	$R11|R10|R9	#restore lino, lc, dp
	jmp	(r8)
l0011:
	ret			#end of interpretation
_GOTO:
	cvtbl	(r10)+,r0
	bneq	l0016
	cvtwl	(r10)+,r0
l0016:
	moval	_display[r0],r0	#r0 has exit dp value
	addl3	(r10),ap,r10		#establish return address
l0017:
	cmpl	r0,r9		#check for done
	beql	l0018
	cmpl	r9,$_display	#check for end of stack
	beql	egoto
	pushl	(r9)		#flush and close local files
	calls	$1,_pclose
	movl	(r9),sp		#deallocate local vars
	movl	4(sp),_file	#restore old I/O info
	movl	8(sp),r7
	movl	16(sp),(r9)	#restore old display entry
	addl2	$20,sp		#deallocate current frame
	movl	(sp),r9		#get old display pointer
	brb	l0017		#continue
l0018:
	movl	*(r9),sp	#reset sp to top of stack
	jmp	(r8)
egoto:
	movl	$EGOTO,_perrno
	jbr	error
_LINO:
	cmpl	*(r9),sp	#check stack integrity
	jneq	stknemp
	cvtbl	(r10)+,r11	#update line number
	bneq	l0015
	cvtwl	(r10)+,r11
l0015:
	aoblss	_stlim,_stcnt,l0021   #update statement count
	movw	$ESTLIM,_perrno
	jbr	error
l0021:
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
	bneq	l0012
	cvtwl	(r10)+,r0
l0012:
	jgtr	framesize	#current maximum of 32768
	mnegl	r0,r0
	blbc	r0,l0020
	incl	r0
l0020:
	subl3	r0,sp,r1	#r1 points to new top of stack
	clrl	tempsize(r1)	#check for memory (fault => not available)
	movl	r1,sp		#allocate the memory
	movc5	$0,(r2),$0,r0,(sp)
	jmp	(r8)
_POP:
	cvtbl	(r10)+,r0
	bneq	l0013
	cvtwl	(r10)+,r0
l0013:
	addl2	r0,sp
	jmp	(r8)
_PUSH4:
	incl	r10
	mnegl	(sp)+,r0
	blbc	r0,l0019
	incl	r0
l0019:
	subl3	r0,sp,r1	#r1 points to new top of stack
	clrl	tempsize(r1)	#check for memeory (fault => not available)
	movl	r1,sp		#allocate the memory
	movc5	$0,(r2),$0,r0,(sp)
	jmp	(r8)
_POP4:
	incl	r10
	addl2	(sp)+,sp
	jmp	(r8)
_SDUP:
	incl	r10
	movw	(sp),-(sp)
	jmp	(r8)
_ASRT:
	incl	r10
	tstw	(sp)+
	beql	l0014
	jmp	(r8)
l0014:
	movw	$EASRT,_perrno
	jbr	error
