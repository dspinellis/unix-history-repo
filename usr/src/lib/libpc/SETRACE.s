# Copyright (c) 1979 Regents of the University of California
#
# sccsid[] = "@(#)SETRACE.s 1.2 3/18/81";
#
# set trace bit in return frame of calling routine
# this will cause core dump at point of return
# unless running a debugger, in which case a breakpoint
# will occur at the error point
#
	.set	PSW,4
	.set	FP,12

	.globl	_SETRACE
_SETRACE:
	.word	0
	movl	FP(fp),r0	#r0 has ptr to callers frame
	bisl2	$0x10,PSW(r0)	#set trace bit
	ret
