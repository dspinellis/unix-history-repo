/ cat -- concatinate files

	mov	(sp)+,r5
	tst	(sp)+
	mov	$obuf,r2
	cmp	r5,$1
	beq	3f

loop:
	dec	r5
	ble	done
	mov	(sp)+,r0
	cmpb	(r0),$'-
	bne	2f
	clr	fin
	br	3f
2:
	mov	r0,0f
	sys	open; 0:..; 0
	bes	loop
	mov	r0,fin
3:
	mov	fin,r0
	sys	read; ibuf; 512.
	bes	3f
	mov	r0,r4
	beq	3f
	mov	$ibuf,r3
4:
	movb	(r3)+,r0
	jsr	pc,putc
	dec	r4
	bne	4b
	br	3b
3:
	mov	fin,r0
	beq	loop
	sys	close
	br	loop

done:
	sub	$obuf,r2
	beq	1f
	mov	r2,0f
	mov	$1,r0
	sys	write; obuf; 0:..
1:
	sys	exit

putc:
	movb	r0,(r2)+
	cmp	r2,$obuf+512.
	blo	1f
	mov	$1,r0
	sys	write; obuf; 512.
	mov	$obuf,r2
1:
	rts	pc

	.bss
ibuf:	.=.+512.
obuf:	.=.+512.
fin:	.=.+2
	.text

