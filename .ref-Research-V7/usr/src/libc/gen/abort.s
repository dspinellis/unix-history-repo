/ C library -- abort

.globl	_abort
iot	= 4
.globl	csv,cret

_abort:
	jsr	r5,csv
	iot
	clr	r0
	jmp	cret
