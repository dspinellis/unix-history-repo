# C library -- killpg

	.set	kill,37
.globl	_killpg
.globl cerror

_killpg:
	.word	0x0000
	mnegl	8(ap),8(ap)		# kill with - signo is killpg
	chmk	$kill
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
