.data
.globl _Y, _OY, _OYcopy
/.globl _Ycopy

_OYcopy:
	mov	$_OY,r0
	mov	$_Y,r1
	mov	r2,-(sp)
/0:
	mov	$7.,r2
1:
	mov	(r1)+,(r0)+
	sob	r2,1b
	mov	(sp)+,r2
	rts	pc
/
/_Ycopy:
/	mov	2(sp),r0
/	mov	4(sp),r1
/	mov	r2,-(sp)
/	br	0b
