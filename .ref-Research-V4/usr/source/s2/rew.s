/ rew -- rewind dec/mag tape

	cmp	(sp)+,$2
	blt	1f
	tst	(sp)+
	mov	(sp)+,r0
	movb	(r0)+,r1
	cmp	r1,$'m
	beq	rewm
	movb	r1,tapx+8
	tstb	(r0)
	bne	error
1:
	sys	open; tapx; 0
	br	rew

rewm:
	movb	(r0)+,r1
	beq	1f
	movb	r1,mtx+7
	tstb	(r0)
	bne	error
1:
	sys	open; mtx; 0

rew:
	bes	error
	sys	read; word; 2
	bes	error
	sys	exit

error:
	mov	$1,r0
	sys	write; 0f; 2
	sys	exit
0:
	<?\n>

tapx:
	</dev/tap0\0>
mtx:
	</dev/mt0\0>
	.even

.bss
word:	.=.+2

