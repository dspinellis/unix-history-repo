/
/

/ a2 -- pdp-11 assembler pass 2

outw:
	cmp	dot-2,$4
	beq	9f
	bit	$1,dot
	bne	1f
	add	$2,dot
	tstb	passno
	beq	8f
	clr	-(sp)
	rol	r3
	adc	(sp)
	asr	r3		/ get relative pc bit
	cmp	r3,$40
	bne	2f
/ external references
	mov	$666,outmod		/ make nonexecutable
	mov	xsymbol,r3
	sub	$usymtab,r3
	asl	r3
	bis	$4,r3		/ external relocation
	br	3f
2:
	bic	$40,r3		/ clear any ext bits
	cmp	r3,$5
	blo	4f
	cmp	r3,$33		/ est. text, data
	beq	6f
	cmp	r3,$34
	bne	7f
6:
	jsr	r5,error; 'r
7:
	mov	$1,r3		/ make absolute
4:
	cmp	r3,$2
	blo	5f
	cmp	r3,$4
	bhi	5f
	tst	(sp)
	bne	4f
	add	dotdot,r2
	br	4f
5:
	tst	(sp)
	beq	4f
	sub	dotdot,r2
4:
	dec	r3
	bpl	3f
	clr	r3
3:
	asl	r3
	bis	(sp)+,r3
	mov	r2,r0
	jsr	r5,putw; txtp
	add	$2,*tseekp
	mov	r3,r0
	jsr	r5,putw; relp
	add	$2,*rseekp
8:
	rts	pc
1:
	jsr	r5,error; 'o
	clr	r3
	jsr	pc,outb
	rts	pc

9:
	jsr	r5,error; 'x
	rts	pc

outb:
	cmp	dot-2,$4		/ test bss mode
	beq	9b
	cmp	r3,$1
	blos	1f
	jsr	r5,error; 'r
1:
	tstb	passno
	beq	2f
	mov	r2,r0
	bit	$1,dot
	bne	1f
	jsr	r5,putw; txtp
	clr	r0
	jsr	r5,putw; relp
	add	$2,*rseekp
	add	$2,*tseekp
	br	2f
1:
	mov	txtp,r0
	movb	r2,-1(r0)
2:
	inc	dot
	rts	pc

error:
	mov	$666,outmod		/ make nonexecutable
	mov	r3,-(sp)
	mov	r2,-(sp)
	mov	r1,-(sp)
	mov	r0,-(sp)
	mov	$argb,r1
1:
	movb	(r1),ch
	beq	1f
	clrb	(r1)+
	mov	$1,r0
	sys	write; ch; 1
	br	1b
1:
	mov	(r5)+,r0
	movb	r0,0f
	mov	line,r3
	mov	$0f+6,r0
	mov	$4,r1
2:
	clr	r2
	dvd	$10.,r2
	add	$'0,r3
	movb	r3,-(r0)
	mov	r2,r3
	sob	r1,2b
	mov	$1,r0
	sys	write; 0f; 7
	mov	(sp)+,r0
	mov	(sp)+,r1
	mov	(sp)+,r2
	mov	(sp)+,r3
	rts	r5

	.data
0:	<f xxxx\n>
	.even
	.text

betwen:
	cmp	r0,(r5)+
	blt	1f
	cmp	(r5)+,r0
	blt	2f
1:
	tst	(r5)+
2:
	rts	r5

