/ monitor-- create mon.out for prof

/	jsr	r5,monitor; lowpc; highpc; buf; bufsize
/
/	jsr	r5,monexit

.globl	monitor
.globl	monexit

profil = 44.

monitor:
	mov	r0,-(sp)
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	(r5)+,r1		/ lo pc
	mov	(r5)+,r2		/ hi pc
	mov	(r5)+,r0		/ buff
	mov	r1,(r0)+
	mov	r2,(r0)+
	mov	r0,buff
	sub	r1,r2
	ble	botch
	mov	(r5)+,r0		/ size
	sub	$4,r0
	ble	botch
	cmp	r0,r2
	blt	1f
	mov	r2,r0
1:
	mov	r0,bufs
	clr	r1
	div	r2,r0
	bvc	1f
	mov	$77777,r0
1:
	asl	r0
	mov	r0,scale
	sys	0; 9f
.data
9:
	sys	profil; buff:.. ; bufs:..; lowpc:..; scale:..
.text
	mov	(sp)+,r2
	mov	(sp)+,r1
	mov	(sp)+,r0
	rts	r5

monexit:
	mov	r0,-(sp)
	sys	creat; monout; 017
	bes	botch
	mov	buff,0f
	sub	$4,0f
	mov	bufs,0f+2
	add	$4,0f+2
	sys	0; 9f
.data
9:
	sys	write; 0:..; ..
.text
	mov	(sp)+,r0
	rts	r5

botch:
	mov	$1,r0
	sys	write; mesg; emesg-mesg
	4

mesg:
	<Monitor botch.\n>
emesg:
monout:
	<mon.out\0>
.even
