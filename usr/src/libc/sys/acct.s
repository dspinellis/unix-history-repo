/ C library -- acct

/ error = acct(string);

.globl	_acct
.globl	cerror
.acct = 51.

_acct:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	sys	0; 9f
	bec	1f
	jmp	cerror
1:
	clr	r0
	mov	(sp)+,r5
	rts	pc
.data
9:
	sys	.acct; 0:..
