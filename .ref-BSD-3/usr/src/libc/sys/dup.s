# C library -- dup

#	f = dup(of [ ,nf])
#	f == -1 for error

	.set	dup,41
.globl	_dup
.globl	_dup2
.globl	cerror

_dup2:
	.word	0x0000
	bisb2	$0100,4(ap)
	brb	L1
_dup:
	.word	0x0000
L1:
	chmk	$dup
	bcc 	noerror
	jmp 	cerror
noerror:
	ret
