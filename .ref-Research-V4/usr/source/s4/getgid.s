/ C library -- getgid

/ gid = getgid();
/

getgid = 47.
.globl	_getgid, retrn

_getgid:
	mov	r5,-(sp)
	mov	sp,r5
	sys	getgid
	bic	$!377,r0
	jmp	retrn
