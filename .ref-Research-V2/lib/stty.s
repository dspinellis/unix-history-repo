/ C library -- stty

/ error = stty(filep, ttyvec);

/ filep is descriptor of open tty
/ ttyvec[0, 1, 2] correspond to args of stty

.globl	_stty

.data
_stty:
	1f
.text
1:
	mov	2(sp),r0
	mov	4(sp),0f
	sys	stty; 0:..
	bes	1f
	clr	r0
	rts	pc
1:
	mov	$1,r0
	rts	pc

