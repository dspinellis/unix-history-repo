/ C return sequence

.globl retrn, cerror, _errno

cerror:
	mov	r0,_errno
	mov	$-1,r0

retrn:
	mov	r5,sp
	mov	(sp)+,r5
	rts	pc
.bss
_errno:	.=.+2
