#
# dummy cleanup routine if none supplied by user.

.globl	__cleanup

__cleanup:
	.word	0x0000
	ret
