/ putw/putc -- write words/characters on output file

	.globl	_putc, _putw, _fflush, _fcreat
	.globl cerror
	.comm	_errno,2

_fcreat:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	mov	6(r5),r1
	mov	pc,(r1)		/ a putatively illegal file desc.
	sys	0; 9f
.data
9:	sys	creat; 0:..; 666
.text
	bes	badret
	mov	r0,(r1)+
	clr	(r1)+
	clr	(r1)+
	br	goodret

_putw:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	mov	6(r5),r1
	mov	r0,-(sp)
	jsr	pc,putc1
	mov	(sp)+,r0
	swab	r0
	jsr	pc,putc1
	br	goodret

_putc:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	mov	6(r5),r1
	jsr	pc,putc1
	mov	4(r5),r0
	br	goodret

putc1:
	dec	2(r1)
	blt	1f
	movb	r0,*4(r1)
	inc	4(r1)
	rts	pc
1:
	mov	r0,-(sp)
	jsr	pc,fl
	mov	(sp)+,r0
	br	putc1

_fflush:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r1
	jsr	pc,fl
	br	goodret

fl:
	mov	r1,r0
	add	$6,r0
	mov	r0,-(sp)
	mov	r0,0f
	mov	4(r1),0f+2
	beq	1f
	sub	(sp),0f+2
	mov	(r1),r0
	sys	0; 9f
.data
9:	sys	write; 0:..; ..
.text
1:
	mov	(sp)+,4(r1)
	mov	$512.,2(r1)
	rts	pc

badret:
	jmp	cerror

goodret:
	clr	_errno
	mov	(sp)+,r5
	rts	pc
