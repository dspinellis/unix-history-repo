/ C library -- gtty

/ error = gtty(filep, ttyvec);

/ filep is descriptor of open tty
/ ttyvec[0, 1, 2] correspond to args of gtty

.globl	_gtty

.data
_gtty:
	1f
.text
1:
	mov	2(sp),r0
	mov	4(sp),0f
	sys	gtty; 0:..
	bes	1f
	clr	r0
	rts	pc
1:
	mov	$1,r0
	rts	r0

