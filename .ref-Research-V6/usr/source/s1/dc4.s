.globl	getchar
.globl	stats
.globl	lookchar
.globl	fsfile
.globl	seekchar
.globl	backspace
.globl	putchar
.globl	alterchar
.globl	move
.globl	rewind
.globl	create
.globl	zero
.globl	allocate
.globl	release
.globl	collect
.globl	getword, putword
.globl	length, position
.globl	w, r, a, l
/
testing = 0
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
/
/
/
/
/	routine to get a word from the string
/	mov	...,r1
/	jsr	pc,getword
/	mov	r0,...
/
getword:
	jsr	pc,getchar
	bes	noch
	movb	r0,nchar
	jsr	pc,getchar
	bec	2f
	dec	r(r1)
	br	noch
2:	movb	r0,nchar+1
	mov	nchar,r0
	tst	r0		/tst clears c-bit
	rts	pc
/
/
/	routine to put a word onto the string
/	mov	...,r1
/	mov	...,r0
/	jsr	pc,putword
/
putword:
	jsr	pc,putchar
	swab	r0
	jsr	pc,putchar
	swab	r0
	rts	pc
/
.bss
nchar:	.=.+2
.text
/
/
/
/	routine to read next character from string
/	pointed to by r1;  character returned in r0
/	c-bit set if character not available (end of file)
/	r1 is preserved
/
/	mov	...,r1
/	jsr	pc,getchar
/	movb	r0,...
/
getchar:
	jsr	pc,lookchar
	bec	2f
	rts	pc
2:	inc	r(r1)
	tst	r0		/tst clears c-bit
	rts	pc
/
noch:	clr	r0
	sec
	rts	pc
/
/	routine to look at next character from string
/	pointed to by r1;  character returned in r0
/	c-bit set if character not available (end of file)
/	r1 is preserved
/
/	mov	...,r1
/	jsr	pc,lookchar
/	movb	r0,...
/
lookchar:
	.if	testing
	jsr	pc,plausible
	inc	stats+6.
	.endif
	cmp	w(r1),r(r1)
	blos	noch
	movb	*r(r1),r0
	clc
	rts	pc
/
plausible:
	cmp	r1,$headers
	bhis 9f; 4; 9:
	cmp	r1,$headend
	blo 9f; 4; 9:
	rts	pc
/
/
/
/	routine to move the read pointer of a string to a
/	specified point.  If the string is not long enough,
/	the string is extended
/
/	mov	position,r0
/	mov	...,r1
/	jsr	pc,seekchar
/
seekchar:
	mov	r1,-(sp)
	mov	r0,-(sp)
	.if	testing
	jsr	pc,plausible
	inc	stats+10.
	.endif
1:
	mov	(sp),r0
	add	a(r1),r0
	cmp	r0,l(r1)
	bhi	3f
	mov	r0,r(r1)
	cmp	r0,w(r1)
	blo	1f
	mov	r0,w(r1)
	br	1f
3:
	mov	(sp),r0
	jsr	pc,allocate
	mov	2(sp),r0
	jsr	pc,move
	jsr	pc,swap
	jsr	pc,release
	mov	2(sp),r1
	br	1b
1:
	mov	(sp)+,r0
	mov	(sp)+,r1
	rts	pc
/
/
/	routine to move read pointer of string to end of string
/
/	mov	...,r1
/	jsr	pc,fsfile
/
fsfile:
	.if	testing
	jsr	pc,plausible
	inc	stats+10.
	.endif
	mov	w(r1),r(r1)
	rts	pc
/
/
/	routine to read a string backwards
/	the read pointer is decremented before reading
/
/	mov	...,r1
/	jsr	pc,backspace
/	mov	r0,...
/
backspace:
	.if	testing
	jsr	pc,plausible
	inc	stats+6.
	.endif
	cmp	a(r1),r(r1)
	bhis	noch
	dec	r(r1)
	movb	*r(r1),r0
	clc
	rts	pc
/
/
/
/
/	routine to put a character into the string
/	pointed to by r1;  character in r0
/	r0 and r1 are preserved.
/
/	movb	ch,r0
/	mov	...,r1
/	jsr	pc,putchar
/
putchar:
	mov	r1,-(sp)
	mov	r0,-(sp)
	.if	testing
	jsr	pc,plausible
	inc	stats+8.
	.endif
1:	cmp	w(r1),l(r1)
	blt	3f
	mov	w(r1),r0
	inc	r0
	sub	a(r1),r0	/W-A+1
	jsr	pc,allocate
	mov	2(sp),r0	/r1
	jsr	pc,move
	jsr	pc,swap
	jsr	pc,release
	mov	2(sp),r1	/r1
	cmp	w(r1),l(r1)
	blt	3f
	jmp	err
/
3:	movb	(sp),*w(r1)
	inc	w(r1)
	mov	(sp)+,r0
	tst	(sp)+
	rts	pc
/
/
swap:
	mov	w(r1),-(sp)
	mov	w(r0),w(r1)
	mov	(sp),w(r0)
	mov	r(r1),(sp)
	mov	r(r0),r(r1)
	mov	(sp),r(r0)
	mov	a(r1),(sp)
	mov	a(r0),a(r1)
	mov	(sp),a(r0)
	mov	l(r1),(sp)
	mov	l(r0),l(r1)
	mov	(sp)+,l(r0)
	rts	pc
/
/
/	routine to alter a character in the string
/	pointed to by r1;  character in r0
/	r0 and r1 are preserved.
/
/	movb	ch,r0
/	mov	...,r1
/	jsr	pc,alterchar
/
alterchar:
	mov	r1,-(sp)
	mov	r0,-(sp)
	.if	testing
	jsr	pc,preposterous
	inc	stats+8.
	.endif
1:	cmp	r(r1),l(r1)
	blt	3f
	mov	l(r1),r0
	inc	r0
	sub	a(r1),r0	/W-A+1
	jsr	pc,allocate
	mov	2(sp),r0	/r1
	jsr	pc,move
	jsr	pc,swap
	jsr	pc,release
	mov	2(sp),r1	/r1
	cmp	r(r1),l(r1)
	blt	3f
	jmp	err
/
3:	movb	(sp),*r(r1)
	inc	r(r1)
	cmp	r(r1),w(r1)
	ble	1f
	mov	r(r1),w(r1)
1:
	mov	(sp)+,r0
	tst	(sp)+
	rts	pc
/
/
/	routine to move the contents of one string
/	to another.
/
/	mov	source,r0
/	mov	dest,r1
/	jsr	pc,move
/
/	on return, r1 points to the new string and should
/	be saved.  r0 is preserved.
/
move:
	mov	r3,-(sp)
	mov	r2,-(sp)
	mov	r1,-(sp)
	mov	r0,-(sp)
	mov	w(r0),r2
	sub	a(r0),r2	/W-A
	mov	l(r1),r3
	sub	a(r1),r3	/L-A
	cmp	r2,r3
	ble	1f
	mov	r2,r0
	jsr	pc,allocate
	mov	2(sp),r0	/r1
	jsr	pc,swap
	jsr	pc,release
	mov	r0,r1
	mov	0(sp),r0	/r0
/
1:	mov	a(r0),(r0)
	mov	a(r1),(r1)
1:	dec	r2
	blt	1f
	movb	*(r0),*(r1)
	inc	(r0)
	inc	(r1)
	br	1b
/
/	fix up read ptr of new string
/
1:	mov	r(r0),r2
	sub	a(r0),r2
	add	a(r1),r2
	mov	r2,r(r1)
/
/	restore and return
/
	mov	(sp)+,r0
	mov	(sp)+,r1
	mov	(sp)+,r2
	mov	(sp)+,r3
	rts	pc
/
/
/	routine to rewind read pointer of string
/	pointed to by r1
/
/	mov	...,r1
/	jsr	pc,rewind
/
rewind:
	.if	testing
	jsr	pc,plausible
	inc	stats+10.
	.endif
	mov	a(r1),r(r1)
	rts	pc
/
/	routine to rewind write pointer of string
/	pointed to by r1
/
/	mov	...,r1
/	jsr	pc,create
/
create:
	.if	testing
	jsr	pc,plausible
	inc	stats+10.
	.endif
	mov	a(r1),w(r1)
	mov	a(r1),r(r1)
	rts	pc
/
/
/	routine to zero a string
/
/	mov	...,r1
/	jsr	pc,zero
/
zero:
	mov	r0,-(sp)
	.if	testing
	jsr	pc,preposterous
	.endif
	mov	a(r1),r0
1:	cmp	r0,l(r1)
	bhis	1f
	clrb	(r0)+
	br	1b
1:	mov	(sp)+,r0
	rts	pc
/
/
