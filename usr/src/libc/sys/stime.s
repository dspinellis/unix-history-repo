	.set	stime,25
.globl	_stime
.globl  cerror

_stime:
	.word	0x0000
	movl	*4(ap),4(ap)	# copy time to set
	chmk	$stime
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
