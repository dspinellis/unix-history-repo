/
/

.globl	main
.globl	temp
.globl	rerr
.globl	fptrap

.comm	erret,2
.comm	argp,2

ldfps	= 170100^tst

	sys	signal; 4; fptrap
	ldfps	$5400
	mov	sp,argp
	mov	$main,r4
	jmp	*(r4)+

rerr:
	mov	(r5)+,r1
	tst	erret
	beq	1f
	jsr	pc,erret
1:
	mov	$temp,r2
	jsr	r5,1f
	movb	$'\n,(r2)+
	sub	$mesg,r2
	mov	r2,0f
	mov	$2,r0
	sys	write; mesg; 0:..
	sys	exit
1:
	clr	r0
	div	$10.,r0
	mov	r1,-(sp)
	mov	r0,r1
	beq	1f
	jsr	r5,1b
1:
	mov	(sp)+,r0
	add	$'0,r0
	movb	r0,(r2)+
	rts	r5

mesg:
	<Runtime error >
temp:	.=.+16.

