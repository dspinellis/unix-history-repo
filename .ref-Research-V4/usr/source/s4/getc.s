/ getw/getc -- get words/characters from input file
/ fopen -- open a file for use by get(c|w)

indir = 0

.globl	_getc, _getw, _fopen
.globl	cerror
.comm	_errno,2

_fopen:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	sys	0; 9f
	.data
9:	sys	open; 0:..; 0
	.text
	bes	badret
	mov	6(r5),r1
	mov	r0,(r1)+
	clr	(r1)+
	clr	r0
	mov	(sp)+,r5
	rts	pc

_getw:
	clr	_errno
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r1
	jsr	pc,getc1
	mov	r0,-(sp)
	jsr	pc,getc1
	swab	r0
	bis	(sp)+,r0
	mov	(sp)+,r5
	rts	pc

_getc:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r1
	jsr	pc,getc1
	mov	(sp)+,r5
	rts	pc

getc1:
	dec	2(r1)
	bge	1f
	mov	r1,r0
	add	$6,r0
	mov	r0,0f
	mov	r0,4(r1)
	mov	(r1),r0
	sys	0; 9f
.data
9:	sys	read; 0:..; 512.
.text
	bes	badret
	tst	r0
	beq	badret
	dec	r0
	mov	r0,2(r1)
1:
	clr	r0
	bisb	*4(r1),r0
	inc	4(r1)
	rts	pc

badret:
	jmp	cerror
