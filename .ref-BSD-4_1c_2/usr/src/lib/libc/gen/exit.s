# @(#) 4.1 82/11/07
# C library -- exit

# exit(code)
# code is return in r0 to system

	.set	exit,1
.globl	_exit
.globl	__cleanup

	.align	1
_exit:
	.word	0x0
	calls	$0,__cleanup
	chmk	$exit
	halt
