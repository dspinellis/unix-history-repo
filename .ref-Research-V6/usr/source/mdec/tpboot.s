/ tape boot program to load and transfer
/ to a 'tp' entry

/ entry is made by jsr pc,*$0
/ so return can be rts pc
/ jsr pc,(r5) is putc
/ jsr pc,2(r5) is getc
/ jsr pc,4(r5) is mesg

core = 24.
.. = [core*2048.]-512.
start:
	mov	$..,sp
	mov	sp,r1
	cmp	pc,r1
	bhis	2f
	clr	r0
	cmp	(r0),$407
	bne	1f
	mov	$20,r0
1:
	mov	(r0)+,(r1)+
	cmp	r1,$core*2048.
	blo	1b
	jmp	(sp)

2:
	mov	$trvect,r5
	mov	$name,r4
	jsr	pc,rew
	mov	$'=,r0
	jsr	pc,(r5)
2:
	mov	r4,r1
1:
	jsr	pc,getc
	cmp	r0,$'\n
	beq	1f
	cmp	r0,$'@
	beq	2b
	movb	r0,(r1)+
	cmp	r0,$'#
	bne	1b
	sub	$2,r1
	cmp	r1,r4
	blo	2b
	br	1b
1:
	clrb	(r1)
	cmp	r1,r4
	blos	start
	mov	$1,tapa
	mov	$-6144.,wc
	jsr	pc,tread
	clr	r1
1:
	mov	r1,r2
	mov	r4,r0
2:
	cmpb	(r0)+,(r1)
	bne	2f
	tstb	(r1)+
	bne	2b
	br	1f
2:
	mov	r2,r1
	add	$64.,r1
	cmp	r1,$12288.
	blo	1b
	jsr	pc,rew
	br	start
1:
	mov	44.(r2),tapa
	mov	38.(r2),r0
	inc	r0
	clc
	ror	r0
	neg	r0
	mov	r0,wc
	clr	r0
1:
	clr	(r0)+
	cmp	r0,sp
	blo	1b
	jsr	pc,tread
	jsr	pc,rew
	br	callout

tapa:	0
wc:	0
ba:	0
name	= ..-32.
