/ C return sequence which
/ sets errno, returns -1.

.globl	cerror
.comm	_errno,2

cerror:
	mov	r0,_errno
	mov	$-1,r0
	mov	r5,sp
	mov	(sp)+,r5
	rts	pc
