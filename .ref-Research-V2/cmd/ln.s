/  link command

ln:
	mov	sp,r5
	cmp	(r5)+,$2
	bhis	1f
	sys	exit
1:
	beq	1f
	tst	(r5)+
	mov	(r5)+,0f
	mov	(r5),0f+2
	br	2f
1:
	tst	(r5)+
	mov	(r5),0f
	mov	(r5),r4
1:
	tstb	(r4)+
	bne	1b
1:
	cmpb	-(r4),$'/
	beq	1f
	cmp	(r5),r4
	bne	1b
	br	err
1:
	inc	r4
	mov	r4,0f+2
2:
	mov	0f,2f
	sys	stat; 2:..; stbuf
	bes	err
	bit	$40000,stbuf+2
	bne	err
	sys	link; 0:..; ..
	bes	err
	sys	exit

err:
	mov	$1,r0
	sys write; quest; 2
	sys	exit

quest:
	<?\n>

.bss
stbuf:	.=.+40.

X

reltm2:
	.byte 0, 0, 0, 0, 0, 0
	.byte 0, M,