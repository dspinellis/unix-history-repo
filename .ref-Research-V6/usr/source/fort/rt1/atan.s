/
/

/ atan & datan fortran functions

.globl	atan.
.globl	datan.

.globl	atan
.globl	retrn
.globl	temp

datan.:	temp
	.+2
	setd
	br	1f

atan.:	temp
	.+2
	setf
1:
	movf	*2(r3),r0
	jsr	pc,atan
	movf	r0,temp
	jmp	retrn
