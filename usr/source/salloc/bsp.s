.globl	backspace
.globl	lookchar
.globl	w, r, a, l
/
/	routine to read a string backwards
/	the read pointer is decremented before reading
/
/	mov	...,r1
/	jsr	pc,backspace
/	mov	r0,...
/
backspace:
	cmp	a(r1),r(r1)
	bhis	nochc
	dec	r(r1)
	jsr	pc,lookchar
	rts	pc
nochc:	clr	r0
	sec
	rts	pc
