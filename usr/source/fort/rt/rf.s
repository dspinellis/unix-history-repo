/
/

/ rf -- real comparisons

.globl	rlt4
.globl	rle4
.globl	req4
.globl	rne4
.globl	rge4
.globl	rgt4

rlt4:
	jsr	pc,compar
	blt	one
	br	zero

rle4:
	jsr	pc,compar
	ble	one
	br	zero

req4:
	jsr	pc,compar
	beq	one
	br	zero

rne4:
	jsr	pc,compar
	bne	one
	br	zero

rge4:
	jsr	pc,compar
	bge	one
	br	zero

rgt4:
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
	mov	(sp)+,r0
	setf
	movf	(sp)+,r0
	cmpf	(sp)+,r0
	cfcc
	jmp	(r0)

