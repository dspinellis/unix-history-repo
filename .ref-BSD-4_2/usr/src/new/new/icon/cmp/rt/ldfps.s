#ifdef VAX
#endif VAX

#ifdef PDP11
/ Load floating-point processor status

.globl	_ldfps
_ldfps:
	ldfps	2(sp)
	rts	pc
#endif PDP11
