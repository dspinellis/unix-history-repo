ldfps = 170100^tst
/
/ ldfps(number);

.globl	_ldfps
_ldfps:
	mov	r5,-(sp)
	mov	sp,r5
	ldfps	4(r5)
	mov	(sp)+,r5
	rts	pc
