/
/

/ alog10 & dlog10 fortran functions

.globl	dlog10.
.globl	alog10.

.globl	log
.globl	retrn
.globl	rerr
.globl	temp

dlog10.:temp
	.+2
	setd
	br	1f

alog10.:temp
	.+2
	setf
1:
	movf	*2(r3),r0
	jsr	pc,log
	bes	1f
	mulf	const,r0
	movf	r0,temp
	jmp	retrn

1:
	jsr	r5,rerr; 1

const:	37736;55730;124467;24146
