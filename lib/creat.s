/ C library -- creat

/ file = creat(string, mode);
/
/ file == -1 if error

	.globl	_creat

.data
_creat:
	1f
.text
1:
	mov	2(sp),0f
	mov	4(sp),0f+2
	sys	creat; 0:..; ..
	bec	1f
	mov	$-1,r0
1:
	rts	pc

