/
/

/ r3 -- comparisons

.globl	lif2
.globl	lif1
.globl	rif4
.globl	iif2
.globl	iif4
.globl	rif8

lif2:
lif1:
	tst	(sp)+
	bne	1f
	mov	(r4),r4
	jmp	*(r4)+
1:
	tst	(r4)+
	jmp	*(r4)+

rif8:
	setd
	tstf	(sp)+
	cfcc
	br	2f

iif4:
rif4:
	mov	(sp)+,r0
	mov	(sp)+,r1
	tst	r0
	bne	2f
	tst	r1
	cln
	br	2f

iif2:
	tst	(sp)+
2:
	bmi	1f
	beq	2f
	tst	(r4)+
2:
	tst	(r4)+
1:
	mov	(r4),r4
	jmp	*(r4)+

