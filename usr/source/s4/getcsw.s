/ C library - getcsw

/ csw = getcsw();

.globl	_getcsw

_getcsw:
	sys	38.
	rts	pc
