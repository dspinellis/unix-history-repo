/ atoi
/ ascii to integer input conversion
/	jsr	r5,atoi; getc

.globl	atoi


atoi:
	clr	r1
	jsr	r5,*(r5)
	clr	-(sp)
	cmp	r0,$'-
	bne	2f
	inc	(sp)
1:
	jsr	r5,*(r5)
2:
	sub	$'0,r0
	cmp	r0,$9
	bhi	1f
	mpy	$10.,r1
	add	r0,r1
	br	1b
1:
	add	$'0,r0
	tst	(sp)+
	beq	1f
	neg	r1
1:
	tst	(r5)+
	rts	r5

