# getpid -- get process ID

	.set	getpid,20
.globl	_getpid

_getpid:
	.word	0x0000
	chmk	$getpid
	ret

.globl	_getppid

_getppid:
	.word	0x0000
	chmk	$getpid
	movl	r1,r0
	ret
