	.asciz  "@(#)dmcount.s	35.1	5/6/81"
# dummy count subroutine called during profiling
# this is what is called if there is any residual profiled code
# which wants to do profiling in a non profiled system.

.globl	mcount
.globl _mcount

_mcount:
mcount:
	rsb		# just leave
