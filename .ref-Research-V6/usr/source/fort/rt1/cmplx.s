/
/

/ cmplx fortran function

.globl	cmplx.
.globl	dcmplx.

.globl	retrn
.globl	temp
.globl	rerr

dcmplx.:temp
	.+2
	setd
	br	1f

cmplx.:	temp
	.+2
	setf
1:
	cmp	*2(sp),$2
	bne	1f
	mov	r3,r1
	tst	(r1)+
	movf	*(r1)+,r0
	movf	*(r1)+,r1
	mov	$temp,r1
	movf	r0,(r1)+
	movf	r1,(r1)+
	jmp	retrn

1:
	jsr	r5,rerr; 6.
