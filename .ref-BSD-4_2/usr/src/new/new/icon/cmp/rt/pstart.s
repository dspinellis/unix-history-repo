#ifdef VAX
.globl	_Pstart
_Pstart:
	ret
#endif VAX

#ifdef PDP11
.globl	_Pstart
_Pstart:
	rts	pc
#endif PDP11
