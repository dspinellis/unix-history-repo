/ kill process

	mov	(sp)+,r4
	tst	(sp)+

loop:
	dec	r4
	ble	done
	mov	(sp)+,r5
	clr	r3
	cmpb	(r5),$'-
	bne	1f
	inc	r5
	clr	signo
1:
	movb	(r5)+,r0
	beq	1f
	sub	$'0,r0
	cmp	r0,$9
	bhi	error
	mul	$10.,r3
	add	r0,r3
	br	1b
1:
	tst	signo
	bne	1f
	tst	r3
	ble	error
	cmp	r3,$12.
	bgt	error
	mov	r3,signo
	br	loop
1:
	mov	r3,r0
	sys	37.; signo: 9.		/ kill
	bec	loop
	mov	r3,r0
	jsr	pc,decml
	mov	$1,r0
	sys	write; m1; em1-m1
	br	loop

error:
	mov	$1,r0
	sys	write; m2; em2-m2
	br	loop

done:
	sys	exit

decml:
	mov	r0,r1
	clr	r0
	div	$10.,r0
	mov	r1,-(sp)
	tst	r0
	beq	1f
	jsr	pc,decml
1:
	mov	(sp)+,r0
	add	$'0,r0
	mov	r0,ch
	mov	$1,r0
	sys	write; ch; 1
	rts	pc

m1:
	<: not found\n>
em1:
m2:
	<non-numeric arg\n>
em2:
.bss
ch:	.=.+2
