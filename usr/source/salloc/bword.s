.globl	backword
.globl	lookchar
.globl	w, r, a, l
/
/
/	routine to get words backwards from string
/
/	mov	...,r1
/	jsr	pc,backword
/	mov	r0,...
/
backword:
	cmp	a(r1),r(r1)
	bhis	nochw
	dec	r(r1)
	jsr	pc,lookchar
	movb	r0,nchar+1
	cmp	a(r1),r(r1)
	bhis	nochw
	dec	r(r1)
	jsr	pc,lookchar
	movb	r0,nchar
	mov	nchar,r0
	rts	pc
/
nochw:
	clr	r0
	sec
	rts	pc
/
nchar:	.=.+2
