/ sum -- check sum file

	mov	(sp)+,r3
	tst	(sp)+
loop:	dec	r3
	bgt	1f
	sys	exit
1:
	clr	bcnt
	mov	(sp)+,0f
	sys	open; 0:..; 0
	bec	3f
	mov	$1,r0
	sys	write; 1f; 2f-1f
	br	loop
1:	<oprd\n>
2:	.even
3:	mov	r0,r1
	clr	r5
1:
	mov	r1,r0
	sys	read; buf; 512.
	bes	err
	tst	r0
	beq	print
	inc	bcnt
	mov	$buf,r2
2:
	movb	(r2)+,r4
	add	r4,r5
	adc	r5
	sob	r0,2b
	br	1b

err:
	mov	$1,r0
	sys	write; 1f; 2

print:
	jsr	pc,decml
	mov	$1,r0
	sys	write; bl; 1
	mov	bcnt,r5
	jsr	pc,decml
	mov	$1,r0
	sys	write; nl; 1
	mov	r1,r0
	sys	close
	br	loop
1:	<? >
nl:	<\n>
bl:	< >
	.even

decml:
	mov	r0,-(sp)
	mov	r1,-(sp)
	mov	r5,r1
	jsr	pc,1f
	mov	(sp)+,r1
	mov	(sp)+,r0
	rts	pc

1:
	clr	r0
	dvd	$10.,r0
	mov	r1,-(sp)
	mov	r0,r1
	beq	1f
	jsr	pc,1b
1:
	mov	(sp)+,ch
	add	$'0,ch
	mov	$1,r0
	sys	write; ch; 1
	rts	pc

.bss
ch:	.=.+2
bcnt:	.=.+2
buf: .=.+512.
