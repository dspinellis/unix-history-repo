.globl	getword
.globl	putword
.globl lookword
.globl alterword
.globl alterchar
.globl	putchar
.globl	lookchar
.globl	w, r, a, l
/
/	routine to put a word onto the string
/	mov	...,r1
/	mov	...,r0
/	jsr	pc,putword
putword:
	jsr	pc,putchar
	swab	r0
	jsr	pc,putchar
	swab	r0
	rts	pc
/
/
/	routine to look at a word from the string
/	mov	...,r1
/	jsr	pc,lookword
/	mov	r0,...
lookword:
	jsr	pc,lookchar
	bes	1f
	movb	r0,nchar
	inc	r(r1)
	jsr	pc,lookchar
	bes	1f
	movb	r0,nchar+1
	dec	r(r1)
	mov	nchar,r0
1:	rts	pc
/
/
/	routine to get a word from the strng
/
getword:
	jsr	pc,lookword
	bes	1f
	add	$2,r(r1)
1:	rts	pc
/
/
/	routine to alter a word in the string
/
alterword:
	jsr	pc,alterchar
	swab	r0
	jsr	pc,alterchar
	swab	r0 
	rts	pc
nchar:	.=.+2
