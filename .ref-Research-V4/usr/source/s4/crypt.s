/ C library -- crypt
/ cstring = crypt(key);

.globl	_crypt
.globl	crypt, retrn, savr5

_crypt:
	mov	r5,-(sp)
	mov	sp,r5
	mov	r5,savr5
	mov	4(r5),r0
	jsr	pc,crypt
	clr	savr5
	jmp	retrn

