# @(#)kill.s	4.1 (Berkeley) 12/21/80
# C library -- kill

	.set	kill,37
.globl	_kill
.globl cerror

_kill:
	.word	0x0000
	chmk	$kill
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
