/
/

/ f17 -- implicit statement

.globl	simpl

.globl	getype
.globl	chrtab
.globl	error

simpl:
	jsr	r5,getype
		br 9f
	mov	r2,r1
	mov	r0,-(sp)
	movb	(r1)+,r0
	cmp	r0,$'(
	beq	1f
	cmpb	r0,$'/
	bne	8f
1:
	movb	(r1)+,r0
	cmpb	chrtab(r0),$2
	bne	8f
	cmpb	r0,$'a
	blo	2f
	sub	$6,r0		/ map 'a into 'Z+1
2:
	movb	(r1)+,r2
	cmpb	r2,$'-
	bne	2f
	movb	(r1)+,r2
	cmpb	chrtab(r2),$2
	bne	8f
	cmp	r2,$'a
	blo	3f
	sub	$6,r2		/ map 'a into 'Z+1
	br	3f
2:
	mov	r0,r2
	dec	r1
3:
	cmp	r0,r2
	bhi	8f
	asl	r0
	asl	r2
3:
	mov	(sp),imptab-[2*'A](r0)
	add	$2,r0
	cmp	r0,r2
	blos	3b
	movb	(r1)+,r0
	cmp	r0,$',
	beq	1b
	tst	(sp)+
	cmp	r0,$')
	beq	1f
	cmp	r0,$'/
	bne	9f
1:
	tstb	(r1)
	bne	simpl
	rts	r5
8:
	tst	(sp)+
9:
	jsr	r5,error; 16.	/ implicit syntax error
	rts	r5

