/
/

/ fortran nice subroutine

.globl	nice.
.globl	retrn, temp

nice.:
	temp
	.+2
	mov	$16.,r0
	tst	*2(sp)
	beq	1f
	mov	2(r3),r0
	mov	2(r0),r0
1:
	sys	nice
	jmp	retrn

