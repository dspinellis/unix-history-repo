/
/

/ r2 -- comparison of 4-byte integers

.globl	ilt4
.globl	ile4
.globl	ieq4
.globl	ine4
.globl	ige4
.globl	igt4
.globl	lan2
.globl	lor2
.globl	lnt2
.globl	ieq2
.globl	ile2
.globl	igt2
.globl	ige2
.globl	leq2
.globl	ine2
.globl	ilt2
.globl	lne2
.globl	leq1
.globl	lne1

ilt4:
	jsr	pc,compar
	blt	one
	br	zero
	blo	one
	br	zero

ile4:
	jsr	pc,compar
	ble	one
	br	zero
	blos	one
	br	zero

ieq4:
	jsr	pc,compar
	beq	one
	br	zero
	beq	one
	br	zero

ine4:
	jsr	pc,compar
	bne	one
	br	zero
	bne	one
	br	zero

ige4:
	jsr	pc,compar
	bge	one
	br	zero
	bhis	one
	br	zero

igt4:
	jsr	pc,compar
	bgt	one
	br	zero
	bhi	one
	br	zero

one:
	mov	$1,(sp)
	jmp	*(r4)+

zero:
	clr	(sp)
	jmp	*(r4)+

compar:
	mov	(sp)+,r0
	mov	(sp)+,r1
	mov	(sp)+,r2
	cmp	(sp)+,r1
	beq	1f
	jmp	(r0)
1:
	cmp	(sp),r2
	jmp	4(r0)

lor2:
	bis	(sp)+,(sp)
	jmp	*(r4)+

lan2:
	tst	(sp)+
	bne	1f
	clr	(sp)
1:
	jmp	*(r4)+

lnt2:
	tst	(sp)
	beq	one
	br	zero

ieq2:
leq2:
	cmp	(sp)+,(sp)
	beq	one
	br	zero

ilt2:	cmp	(sp)+,(sp)
	bgt	one
	br	zero

ile2:
	cmp	(sp)+,(sp)
	bge	one
	br	zero

igt2:
	cmp	(sp)+,(sp)
	blt	one
	br	zero

ige2:
	cmp	(sp)+,(sp)
	ble	one
	br	zero

ine2:
lne2:
	cmp	(sp)+,(sp)
	bne	one
	br	zero

leq1:
	cmpb	(sp)+,(sp)
	beq	one
	br	zero

lne1:
	cmpb	(sp)+,(sp)
	bne	one
	br	zero

