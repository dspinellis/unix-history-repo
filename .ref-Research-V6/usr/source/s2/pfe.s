/ print last floating error

stst	= 170300^tst

	stst	r1
	cmp	r1,$14
	blos	1f
	clr	r1
1:
	bic	$1,r1
	mov	mesg(r1),r1
1:
	movb	(r1)+,ch
	beq	1f
	mov	$1,r0
	sys	write; ch; 1
	br	1b
1:
	sys	exit

mesg:
	1f
	2f
	3f
	4f
	5f
	6f
	7f
	8f

1:	<No error.\n\0>
2:	<Floating op code error\n\0>
3:	<Floating divide check\n\0>
4:	<Integer conversion error\n\0>
5:	<Floating overflow\n\0>
6:	<Floating underflow\n\0>
7:	<Floating undefined\n\0>
8:	<Floating maintennace trap\n\0>

	.even

	.bss
ch:	.=.+2

