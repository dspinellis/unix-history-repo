# @(#)cerror.s	4.1 (Berkeley) 12/21/80
# C return sequence which
# sets errno, returns -1.

.globl	cerror
.comm	_errno,4

cerror:
	movl	r0,_errno
	mnegl	$1,r0
	ret
