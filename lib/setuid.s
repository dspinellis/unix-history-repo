/ C library -- setuid

/ error = setuid(uid);

	.globl	_setuid

.data
_setuid:
	1f
.text
1:
	mov	2(sp),r0
	sys	setuid
	bec	1f
	mov	$1,r0
	rts	pc
1:
	clr	r0
	rts	pc

