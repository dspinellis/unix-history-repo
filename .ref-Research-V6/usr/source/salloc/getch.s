.globl	getchar
.globl	lookchar
.globl	w, r, a, l
/
/
/	routine to read next character from string
/	pointer to by r1; character returned in r0
/	c-bit set if character not availiable (eof)
/
/	mov	...,r1
/	jsr	pc,getchar
/	movb	r0,...
/
getchar:
	jsr	pc,lookchar
	bes	1f
	inc	r(r1)
	tst	r0
1:	rts	pc
