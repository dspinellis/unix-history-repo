/
/

/ re -- comparison of 8-byte quantities

.globl	rle8
.globl	rlt8
.globl	req8
.globl	rne8
.globl	rge8
.globl	rgt8
rlt8:
	jsr	pc,compar
	blt	one
	br	zero

rle8:
	jsr	pc,compar
	ble	one
	br	zero

req8:
	jsr	pc,compar
	beq	one
	br	zero

rne8:
	jsr	pc,compar
	bne	one
	br	zero

rge8:
	jsr	pc,compar
	bge	one
	br	zero

rgt8:
	jsr	pc,compar
	bgt	one
	br	zero

one:
	mov	$1,-(sp)
	jmp	*(r4)+

zero:
	clr	-(sp)
	jmp	*(r4)+

compar:
	setd
	mov	(sp)+,r0
	movf	(sp)+,fr0
	cmpf	(sp)+,fr0
	cfcc
	jmp	(r0)

