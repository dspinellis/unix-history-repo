/
/

/ fxa -- genop -- output a typed operator

.globl	genop
.globl	newline

.globl	code
.globl	typ

genop:
	mov	r1,-(sp)
	mov	r3,-(sp)
	bic	$!7,r3
	movb	typ(r3),r1
	mov	(sp),r3
	clrb	r3
	swab	r3
	jsr	r5,code
		<	%c%c%d\0>; .even
		r1
		r0
		r3
	mov	(sp)+,r3
	mov	(sp)+,r1
	rts	r5

newline:
	jsr	r5,code
		<\n\0>; .even
	rts	r5

typ:
	<?irlc??g>

