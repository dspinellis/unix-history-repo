/ getchar() -- get a character from input file fin

.globl	_getchar
.comm	_errno,2
.comm	_fin,518.

_getchar:
	mov	r5,-(sp)
	mov	sp,r5
	dec	_fin+2
	bge	1f
	mov	$_fin+6,_fin+4
	mov	_fin,r0
	sys	read; _fin+6; 512.
	bes	badret
	tst	r0
	beq	badret
	dec	r0
	mov	r0,_fin+2
1:
	clr	r0
	bisb	*_fin+4,r0
	inc	_fin+4
	mov	(sp)+,r5
	rts	pc

badret:
	mov	r0,_errno
	clr	r0
	clr	_fin+2
	mov	(sp)+,r5
	rts	pc
