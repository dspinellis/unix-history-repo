/ C library -- getuid

/ uid = getuid();
/

.globl	_getuid, retrn

_getuid:
	mov	r5,-(sp)
	mov	sp,r5
	sys	getuid
	bic	$!377,r0
	jmp	retrn
