.globl .p,sprv
.globl .eq,.ne,.lt,.le,.gt,.ge

.eq:
	jsr	pc,sprv
	cmp	4(sp),(sp)
	beq	true
	br	false

.ne:
	jsr	pc,sprv
	cmp	4(sp),(sp)
	bne	true
	br	false
.lt:
	jsr	pc,sprv
	cmp	4(sp),(sp)
	blt	true
	br	false

.le:
	jsr	pc,sprv
	cmp	4(sp),(sp)
	ble	true
	br	false

.gt:
	jsr	pc,sprv
	cmp	4(sp),(sp)
	bgt	true
	br	false

.ge:
	jsr	pc,sprv
	cmp	4(sp),(sp)
	bge	true
	br	false

true:
	mov	$1,4(sp)
	jmp	.p

false:
	clr	4(sp)
	jmp	.p
