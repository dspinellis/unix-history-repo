/ copy mag tape to disk
/ load with proper tape and disk drivers

	jsr	pc,rew
2:
	jsr	pc,4(r5)
		<disk offset\n\0>
		.even
	jsr	pc,numb
	mov	r0,dska
	jsr	pc,4(r5)
		<tape offset\n\0>
		.even
	jsr	pc,numb
	mov	r0,tapa
	jsr	pc,4(r5)
		<count\n\0>
		.even
	jsr	pc,numb
	mov	r0,r2
1:
	jsr	pc,tread
	jsr	pc,wblk
	inc	tapa
	inc	dska
	dec	r2
	bne	1b
	jsr	pc,rew
	rts	pc

numb:
	clr	r1
1:
	jsr	pc,2(r5)
	cmp	r0,$'\n
	beq	1f
	sub	$'0,r0
	cmp	r0,$9
	bhi	2f
	mul	$10.,r1
	add	r0,r1
	br	1b
1:
	mov	r1,r0
	rts	pc
2:
	jsr	pc,4(r5)
		<illegal digit\n\0>
		.even
	tst	(sp)+
	rts	pc

ba:	buf
wc:	-256.
.bss
buf:	.=.+512.
dska:	.=.+2
tapa:	.=.+2
.text
