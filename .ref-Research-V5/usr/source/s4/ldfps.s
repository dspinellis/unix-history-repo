ldfps = 170100^tst
/
/ ldfps(number);

.globl	_ldfps
_ldfps:
	ldfps	2(sp)
	rts	pc
