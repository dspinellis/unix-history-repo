.globl	length
.globl	position
.globl	w, r, a, l
/
/
/	routine to return the length of a string
/
/	mov	...,r1
/	jsr	pc,length
/	mov	r0,...
/
length:
	mov	w(r1),r0
	sub	a(r1),r0
	rts	pc
/
/
/	routine to return the read pointer position
/
/	mov	...,r1
/	jsr	pc,position
/	mov	r0,...
/
position:
	mov	r(r1),r0
	sub	a(r1),r0
	rts	pc
