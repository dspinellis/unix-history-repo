/ call getarg(n, array [, nword] )
/
/ fill in the array with the n-th argument to the routine.
/ array is integer.
/ n counts from 1 for the file name (first) arg
/ if nword is specifies it gives the max number
/ of words to fill in
/ If no nword is given the array ends with at least one blank.
/ if nword is given the array is blank-padded to that length.

/ iargc(dummy) returns the number of arguments
/ (1-origin)

.globl	getarg., iargc.
.globl	retrn, temp
.comm	argp,2

getarg.:
	temp
	.+2
	mov	$512.,r2
	cmp	*2(sp),$3
	blt	1f
	mov	6(r3),r2
	mov	2(r2),r2
	asl	r2
	asl	r2
1:
	mov	4(r3),r4
	mov	2(r3),r1
	mov	2(r1),r1
	mov	argp,r0
	cmp	r1,(r0)
	bgt	1f
	asl	r1
	add	r0,r1
	mov	(r1),r1
2:
	tst	r2
	ble	1f
	movb	(r1)+,(r4)+
	beq	2f
	dec	r2
	br	2b
2:
	dec	r4
1:
	movb	$' ,(r4)+
	dec	r2
	bit	$3,r2
	bne	1b
	cmp	r2,$256.
	bge	1f
	tst	r2
	bgt	1b
1:
	jmp	retrn

iargc.:
	temp
	.+2
	clr	temp
	mov	*argp,temp+2
	jmp	retrn
