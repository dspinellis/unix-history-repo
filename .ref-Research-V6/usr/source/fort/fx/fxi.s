/
/

/ quicker sort

/	mov	$base,r1
/	mov	$base+[n*width],r2
/	mov	$width,r3
/	jsr	pc,qsort

/	r0,r1,r2,r3,r4 are used

.globl	qsort
.globl	compare

qsort:
	mov	r5,-(sp)
	mov	r4,-(sp)
	bit	$1,r3
	bne	1f
	bit	$1,r1
	bne	1f
	cmp	r3,$2
	bne	2f
	mov	$exch1,exchange
	br	3f
2:
	mov	r3,r5
	clc
	ror	r5
	mov	r5,width
	mov	$exchw,exchange
	br	3f
1:
	mov	$exchb,exchange
3:
	jsr	pc,qs1
	mov	(sp)+,r4
	mov	(sp)+,r5
	rts	pc

qs1:
	mov	r1,r5
	neg	r5
	add	r2,r5
	bgt	1f
	rts	pc
1:
	clr	r4
	dvd	r3,r4
	asr	r4
	mpy	r3,r4
	mov	r5,r4
	add	r1,r4

	mov	r1,-(sp)
	mov	r2,-(sp)

loop:
	cmp	r1,r4
	bhis	loop1
	mov	r1,r0
	jsr	pc,compare
	bgt	loop1
	add	r3,r1
	br	loop

loop1:
	cmp	r2,r4
	blos	1f
	sub	r3,r2
	mov	r2,r0
	jsr	pc,compare
	bge	loop1

	jsr	pc,*exchange
	cmp	r1,r4
	bne	loop
	mov	r2,r4
	br	loop

1:
	cmp	r1,r4
	beq	1f
	jsr	pc,*exchange
	mov	r1,r4
	br	loop1

1:
	mov	(sp)+,r2
	mov	r4,-(sp)
	mov	r4,r1
	add	r3,r1
	mov	r2,r0
	sub	r1,r0
	sub	2(sp),r4
	cmp	r0,r4
	blo	1f
	mov	(sp)+,r0
	mov	(sp)+,r4
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r0,r2
	mov	r4,r1
1:
	jsr	pc,qs1
	mov	(sp)+,r2
	mov	(sp)+,r1
	br	qs1

exchb:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,r5
1:
	movb	(r1),r0
	movb	(r2),(r1)+
	movb	r0,(r2)+
	sob	r5,1b
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	pc

exchw:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	width,r5
1:
	mov	(r1),r0
	mov	(r2),(r1)+
	mov	r0,(r2)+
	sob	r5,1b
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	pc

exch1:
	mov	(r1),r0
	mov	(r2),(r1)
	mov	r0,(r2)
	rts	pc

.bss
exchange: .=.+2
width:	.=.+2
