.globl	plausible
.globl	lookchar
.globl	wc
.globl	seekchar
.globl	putchar
.globl	allocate
.globl	release
.globl	stats
.globl	w,r,a,l
.globl	bufchar
.globl	copy
.globl	getbuf
.globl	swap
.globl fixct
/
/	routine to copy the contents of one string
/	to another.
/
/	mov	source,r0
/	mov	dest,r1
/	jsr	pc,copy
/	mov	r1,...
/
/	on return, r1 points to the new string and should
/	be saved.  r0 is preserved.
/
copy:
	mov	r0,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	jsr	pc,wc
1:	jsr	pc,reset
	mov	w(r0),r2
	sub	a(r0),r2	/W-A
	mov	l(r1),r3
	sub	a(r1),r3	/L-A
	cmp	r2,r3
	ble	1f
	jsr	pc,release
	mov	r2,r0
	jsr	pc,allocate
	mov	4(sp),r0
/
1:	mov	a(r1),w(r1)	/rewind w pointer
	mov	a(r0),-(sp)
/
4:	mov	(sp),0f
	mov	afi,r0
	sys	seek;0:.. ;0	/set input pointer
	cmp	r2,$512.
	ble	2f
	mov	$512.,r3	/# output this time
	mov	r3,0f
	mov	r3,3f
	add	r3,(sp)
	sub	r3,r2	/# left to output
	br	1f
/
2:	mov	r2,0f
	mov	r2,3f
	mov	r2,r3
	clr	r2
/
1:	mov	afi,r0
	sys	read;b1;0:..
	bes	bad
	cmp	r0,r3
	bne	bad
	mov	afout,r0
	mov	(r1),0f
	add	r3,(r1)
	sys	seek;0:.. ;0
	sys	write;b1;3:..
	bes	bad
	tst	r2
	bgt	4b
	tst	(sp)+
	mov	4(sp),r0	/restore r0
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
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r0
	rts	pc
/
bad:	mov	$1,r0
	sys write;1f;2f-1f
	4
1:	<error on copy\n>
2:	.even
/
/
wc:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	clr	r3
1:
	cmp	r3,nbuf
	bge	1f
	mov	r3,r2
	asl	r2
	tst	w1(r2)
	ble	2f
	mov	r3,r1
	ashc	$9.,r1
	bic	$777,r1
	add	$b1,r1
	jsr	pc,clean
2:
	inc	r3
	br	1b
1:
	jsr	pc,whead
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	pc
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
/
/
/
/	mov	position,r0
/	mov	...,r1
/	jsr	pc,seekchar
/
seekchar:
	mov	r1,-(sp)
	mov	r0,-(sp)
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
	jsr	pc,copy
	jsr	pc,swap
	jsr	pc,release
	mov	2(sp),r1
	br	1b
1:
	mov	(sp)+,r0
	mov	(sp)+,r1
	rts	pc
/
reset:
	mov	r3,-(sp)
	mov	r2,-(sp)
	clr	r3
1:
	cmp	r3,nbuf
	bge	1f
	mov	r3,r2
	asl	r2
	mov	$-1.,w1(r2)
	clr	b1s(r2)
	clr	b1e(r2)
	clr	u1(r2)
	inc	r3
	br	1b
1:
	clr	flag
	mov	(sp)+,r2
	mov	(sp)+,r3
	rts	pc
/
