/
/

/ cexp fortran function

.globl	cexp.

.globl	rval8p
.globl	retrn
.globl	temp
.globl	rerr
.globl	exp
.globl	sin
.globl	cos

cexp.:	temp
	rval8p; 2
	.+2
	setf
	movf	(sp)+,r0
	jsr	pc,exp
	bes	1f
	movf	r0,temp
	movf	(sp),r0
	jsr	pc,sin
	mulf	temp,r0
	movf	r0,temp+4
	movf	(sp)+,r0
	jsr	pc,cos
	mulf	temp,r0
	movf	r0,temp
	jmp	retrn

1:
	jsr	r5,rerr; 5.
