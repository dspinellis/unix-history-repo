# Copyright (c) 1979 Regents of the University of California
#
# sccsid[] = "@(#)GOTO.s 1.3 6/10/81";
#
	.set	EGOTO,13	#this is dependent upon errdata
	.set	PC,16

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
	pushl	$0
	pushal	L3
	calls	$2,_ERROR
	ret
L3:
	.asciz	"Active frame not found in non-local goto\n"
