# C library -- _exit

# _exit(code)
# code is return in r0 to system
# Same as plain exit, for user who want to define their own exit.

	.set	exit,1
.globl	__exit

	.align	1
__exit:
	.word	0x0000
	chmk	$exit
	halt
