/
/ 32-bit multiplication routine for fixed pt hardware.
/  Implements * operator
/ Credit to an unknown author who slipped it under the door.
.globl	lmul
.globl	csv, cret

lmul:
	jsr	r5,csv
	mov	6(r5),r2
	sxt	r1
	sub	4(r5),r1
	mov	10.(r5),r0
	sxt	r3
	sub	8.(r5),r3
	mul	r0,r1
	mul	r2,r3
	add	r1,r3
	mul	r2,r0
	sub	r3,r0
	jmp	cret
