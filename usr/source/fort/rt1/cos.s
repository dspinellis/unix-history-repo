/
/

/ cos & dcos fortran functions

.globl	cos.
.globl	dcos.

.globl	cos
.globl	retrn
.globl	temp

dcos.:	temp
	.+2
	setd
	br	1f

cos.:	temp
	.+2
	setf
1:
	movf	*2(r3),r0
	jsr	pc,cos
	movf	r0,temp
	jmp	retrn
