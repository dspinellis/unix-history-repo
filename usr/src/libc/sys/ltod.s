# C library
# return floating-point from long integer
#	d = ltod(l)

.globl	_ltod
_ltod:
	.word	0x0000
	cvtld	4(ap),r0
	ret

# return long integer from floating
#	dtol(d, l)

.globl	_dtol
_dtol:
	.word	0x0000
	cvtdl	4(ap),*12(ap)
	ret
