.globl	alterchar
.globl	plausible
.globl	stats
.globl	allocate, release
.globl	copy
.globl	bufchar, getbuf
.globl	swap
.globl	w, r, a, l
.globl	w1, flag, u1, fixct
/
/	routine to rewrite the character pointer to by
/	read pointer.  The read pointer is advanced.
/	the string is extended if necessary.
/	there is no error return.
/
/	movb	ch,r0
/	mov	...,r1
/	jsr	pc,alterchar
/	mov	r1,...
/
alterchar:
	mov	r2,-(sp)
	mov	r1,-(sp)
	mov	r0,nchar
	jsr	pc,plausible
	inc	stats+8.
1:	cmp	r(r1),l(r1)	/W,L
	blt	3f
	mov	l(r1),r0
	inc	r0
	sub	a(r1),r0	/W-A+1
	jsr	pc,allocate
	mov	(sp),r0
	jsr	pc,copy
	jsr	pc,swap
	jsr	pc,release
	mov	(sp),r1
/
3:	mov	r(r1),r0
	jsr	pc,bufchar
	bec	2f
	jsr	pc,getbuf

2:	movb	nchar,(r0)
	mov	nchar,r0	/to preserve r0 for user
	inc	r(r1)
	mov	$1,w1(r2)
	cmp	r(r1),w(r1)
	ble	3f
	mov	r(r1),w(r1)
3:
	inc	flag
	bne	1f
	jsr	pc,fixct
	br	2f
1:
	mov	flag,u1(r2)
2:
	mov	(sp)+,r1
	mov	(sp)+,r2
	rts	pc
/
nchar:	.=.+2
