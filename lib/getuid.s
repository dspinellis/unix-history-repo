/ C library -- getuid

/ uid = getuid();
/
/ uid == -1 if error

	.globl	_getuid

.data
_getuid:
	1f
.text
1:
	sys	getuid
	bec	1f
	mov	$-1,r0
	rts	pc
1:
	bic	$!377,r0
	rts	pc

