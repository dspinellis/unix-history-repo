/
/

/ ra -- fortran runtime --  real ** real

.globl	rpr4
.globl	rpr8

.globl	log
.globl	exp
.globl	rerr

rpr8:
	setd
	br	1f

rpr4:
	setf

1:
	seti
	movf	(sp)+,fr1
	movf	(sp)+,fr0
	cfcc
	beq	3f			/ 0**x
	movf	fr1,-(sp)
	jsr	pc,log
	bes	error
	mulf	(sp)+,fr0
	jsr	pc,exp
	bes	error
2:
	movf	fr0,-(sp)
	jmp	*(r4)+

3:
	tstf	r1
	cfcc
	bgt	2b

error:
	jsr	r5,rerr; 17.

