/ C library -- ascii to floating

/	f = atof(p)
/	char *p;

ldfps = 170100^tst
stfps = 170200^tst

.globl	_atof

.globl	csv, cret

_atof:
	jsr	r5,csv
	stfps	-(sp)
	ldfps	$200
	movf	fr1,-(sp)
	clr	-(sp)
	clrf	fr0
	clr	r2
	mov	4(r5),r3
1:
	movb	(r3)+,r0
	cmp	$' ,r0
	beq	1b
	cmpb	r0,$'-
	bne	2f
	inc	(sp)
1:
	movb	(r3)+,r0
2:
	sub	$'0,r0
	cmp	r0,$9.
	bhi	2f
	jsr	pc,digit
		br 1b
	inc	r2
	br	1b
2:
	cmpb	r0,$'.-'0
	bne	2f
1:
	movb	(r3)+,r0
	sub	$'0,r0
	cmp	r0,$9.
	bhi	2f
	jsr	pc,digit
		dec r2
	br	1b
2:
	cmpb	r0,$'E-'0
	beq	3f
	cmpb	r0,$'e-'0
	bne	1f
3:
	clr	r4
	clr	r1
	cmpb	(r3),$'-
	bne	3f
	inc	r4
	inc	r3
3:
	movb	(r3)+,r0
	sub	$'0,r0
	cmp	r0,$9.
	bhi	3f
	mul	$10.,r1
	add	r0,r1
	br	3b
3:
	tst	r4
	bne	3f
	neg	r1
3:
	sub	r1,r2
1:
	movf	$one,fr1
	mov	r2,-(sp)
	beq	2f
	bgt	1f
	neg	r2
1:
	cmp	r2,$38.
	blos	1f
	clrf	fr0
	tst	(sp)+
	bmi	out
	movf	$huge,fr0
	br	out
1:
	mulf	$ten,fr1
	sob	r2,1b
2:
	tst	(sp)+
	bge	1f
	divf	fr1,fr0
	br	2f
1:
	mulf	fr1,fr0
	cfcc
	bvc	2f
	movf	$huge,fr0
2:
out:
	tst	(sp)+
	beq	1f
	negf	fr0
1:
	movf	(sp)+,fr1
	ldfps	(sp)+
	jmp	cret
/
/
digit:
	cmpf	$big,fr0
	cfcc
	blt	1f
	mulf	$ten,fr0
	movif	r0,fr1
	addf	fr1,fr0
	rts	pc
1:
	add	$2,(sp)
	rts	pc
/
/
one	= 40200
ten	= 41040
big	= 56200
huge	= 77777
