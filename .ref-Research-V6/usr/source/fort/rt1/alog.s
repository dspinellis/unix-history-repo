/
/

/ alog & log fortran functions

.globl	alog.
.globl	dlog.

.globl	log
.globl	retrn
.globl	rerr
.globl	temp

dlog.:	temp
	.+2
	setd
	br	1f

alog.:	temp
	.+2
	setf
1:
	seti
	movf	*2(r3),r0
	jsr	pc,log
	bes	1f
	movf	r0,temp
	jmp	retrn

1:
	jsr	r5,rerr; 1
