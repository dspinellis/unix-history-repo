.globl	zero
.globl	putchar
.globl	w, r, a, l
/
/
/	routine to zero a string
/
/	mov	...,r1
/	jsr	pc,zero
/
zero:
	mov	r0,-(sp)
	mov	a(r1),w(r1)
	clrb	r0
1:	cmp	w(r1),l(r1)
	bhis	1f
	jsr	pc,putchar
	br	1b
1:	mov	a(r1),w(r1)
	mov	(sp)+,r0
	rts	pc
