# @(#)execl.s	4.1 (Berkeley) 12/21/80
# C library -- execl

# execl(file, arg1, arg2, ... , 0);
#

.globl	_execl

_execl:
	.word	0x0000
	pushab	8(ap)
	pushl	4(ap)
	calls	$2,_execv
	ret
