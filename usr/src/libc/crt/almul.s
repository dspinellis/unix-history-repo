/
/ 32-bit multiplication routine for fixed pt hardware.
/  Implements *= operator
/ Credit to an unknown author who slipped it under the door.
.globl	almul
.globl	csv, cret

almul:
	jsr	r5,csv
	mov	4(r5),r4
	mov	2(r4),r2
	sxt	r1
	sub	(r4),r1
	mov	8.(r5),r0
	sxt	r3
	sub	6.(r5),r3
	mul	r0,r1
	mul	r2,r3
	add	r1,r3
	mul	r2,r0
	sub	r3,r0
	mov	r0,(r4)+
	mov	r1,(r4)
	jmp	cret
