/ C library -- open

/ file = open(string, mode)
/
/ file == -1 means error

	.globl	_open

.data
_open:
	1f
.text
1:
	mov	2(sp),0f
	mov	4(sp),0f+2
	sys	open; 0:..; ..
	bec	1f
	mov	$-1,r0
1:
	rts	pc

