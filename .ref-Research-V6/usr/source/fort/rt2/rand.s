/ fortran random number generator.
/ either single or double prec.

.globl	rand., srand.
.globl	retrn

srand.:
	value
	.+2
	mov	*2(r3),seed1
	inc	first
	jmp	retrn

rand.:
	value
	.+2
	tst	first
	bne	1f
	sys	time
	mov	r1,seed1
	inc	first
1:
	mov	seed1,r1
	jsr	pc,ran
	mov	r1,seed1
	seti
	setd
	movif	r0,fr0
	divf	$44000,fr0
	movf	fr0,value
	jmp	retrn

ran:
	mpy	$13077.,r1
	add	$6925.,r1
	mov	r1,r0
	bic	$100000,r0
	rts	pc

.bss
first:	.=.+2
seed1:	.=.+2
value:	.=.+8
