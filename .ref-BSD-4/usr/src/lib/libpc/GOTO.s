# Copyright (c) 1979 Regents of the University of California
#
# sccsid[] = "@(#)GOTO.s 1.1 10/29/80";
#
	.data
jmplbl:
	.long	0
frame:
	.long	0
	.text

	.globl	_GOTO
	.align	1
_GOTO:
	.word	0
	movl	*4(ap),frame	#save parameters
	movl	PC(fp),jmplbl
	moval	unwind,PC(fp)	#begin unwinding
	ret
unwind:
	tstl	(fp)		#check for exception vector
	bneq	L1
	cmpl	ap,__disply+8	#check for past global procedure
	bgequ	egoto
	moval	unwind,PC(fp)	#blow away this stack frame
	ret
L1:
	cmpl	ap,frame	#check for requested frame
	bgtru	egoto		#lost it somewhere
	blssu	L2		#not there yet
	jmp	*jmplbl		#proceed in this section
L2:
	pushl	-4(fp)		#level of this frame
	calls	$1,*(fp)	#call the exception handler
	movq	-12(fp),*-4(fp)	#restore the display
	moval	unwind,PC(fp)	#blow away this stack frame
	ret
egoto:
	pushl	$EGOTO
	calls	$1,_ERROR
	ret
