/
/

/ r6 -- real arithmetic

.globl	rad4
.globl	rad8
.globl	rsb4
.globl	rsb8
.globl	rmp4
.globl	rmp8
.globl	rdv4
.globl	rdv8
.globl	rng4
.globl	rng8
.globl	r4r8
.globl	r8r4
.globl	i4r4
.globl	i2r4
.globl	r4i4
.globl	r8i2
.globl	i2r8
.globl	r4i2
.globl	i4r8
.globl	r8i4
.globl	rerr

rad8:
	setd
	br	1f
rad4:
	setf
1:
	movf	(sp)+,fr0
	addf	(sp)+,fr0
	br	store

rsb8:
	setd
	br	1f
rsb4:
	setf
1:
	movf	(sp)+,fr0
	negf	fr0
	addf	(sp)+,fr0
	br	store

rmp8:
	setd
	br	1f
rmp4:
	setf
1:
	movf	(sp)+,fr0
	mulf	(sp)+,fr0
	br	store

rdv8:
	setd
	br	1f
rdv4:
	setf
1:
	movf	(sp)+,fr1
	movf	(sp)+,fr0
	divf	fr1,fr0

store:
	movf	fr0,-(sp)
	jmp	*(r4)+

rng8:
	setd
	br	1f
rng4:
	setf
1:
	negf	(sp)
	jmp	*(r4)+

r8r4:
	setd
	br	1f
r4r8:
	setf
1:
	movf	(sp)+,fr0
	movfo	fr0,-(sp)
	jmp	*(r4)+

i2r8:
	seti
	br	1f

i4r8:
	setl
1:
	setd
	br	2f

i2r4:
	seti
	br	1f

i4r4:
	setl
1:
	setf
2:
	movif	(sp)+,fr0
	movf	fr0,-(sp)
	jmp	*(r4)+

r8i2:
	seti
	br	1f

r8i4:
	setl
1:
	setd
	br	2f

r4i2:
	seti
	br	1f

r4i4:
	setl
1:
	setf
2:
	movf	(sp)+,fr0
	movfi	fr0,-(sp)
	jmp	*(r4)+

