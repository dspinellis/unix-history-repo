/ C library -- unlink

/ error = unlink(string);
/

.globl	_unlink, retrn, cerror

_unlink:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	sys	0; 9f
	bec	1f
	jmp	cerror
1:
	clr	r0
	jmp	retrn
.data
9:
	sys	unlink; 0:..
