/ return name of current tty

.globl	ttyn, _ttyn

_ttyn:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	jsr	pc,1f
	mov	(sp)+,r5
	rts	pc

ttyn:
	clr	r0
1:
	mov	$'x,name
	tst	-(sp)
	sys	fstat; buf
	bes	er1
	mov	buf+2,(sp)
	sys	open; dev; 0
	bes	er1
	mov	r0,r1
1:
	mov	r1,r0
	sys	read; buf; 16.
	bes	er
	cmp	r0,$16.
	bne	er
	mov	$buf,r0
	cmp	(r0)+,(sp)
	bne	1b
	cmp	(r0)+,$"tt
	bne	1b
	cmpb	(r0)+,$'y
	bne	1b
	tstb	(r0)+
	beq	1b
	cmpb	(r0),$'\0
	bne	1b
	movb	-(r0),name

er:
	mov	r1,r0
	sys	close

er1:
	tst	(sp)+
	movb	name,r0
	rts	pc

.data
dev:	</dev\0>
.even
.bss
buf:	.=.+40.
name:	.=.+2
