.globl	_pwril, _pwrite, csv, cret
.text
/
/ pwril(w, lint)
/ int w;
/ long lint;
/
/ This routine writes the character representation of lint
/ in a field at least w characters wide using pwrite.
/
_pwril:
	jsr	r5,csv
	mov	sp,r3
	add	$14.,r3			/ 2(r3) = low, (r3) = high
	mov	sp,r2
	sub	$14.,sp
	clr	(r2)
	mov	(r3),r1
	bpl	pwril
	neg	r1
	neg	2(r3)
	sbc	r1
	inc	(r2)
pwril:
	clr	r0
	div	$10.,r0
	mov	r0,(r3)
	mov	r1,r0
	asl	r0
	asl	r0
	add	$pw_tab,r0
	mov	(r0)+,r1
	mov	r0,-(sp)
	clr	r0
	add	2(r3),r1
	adc	r0
	div	$10.,r0
	add	$'0,r1
	movb	r1,-(r2)
	add	*(sp)+,r0
	adc	(r3)
	beq	1f
	mov	r0,2(r3)
	mov	(r3),r1
	br	pwril
1:
	mov	r0,r1
	beq	1f
pwrsl:
	clr	r0
	div	$10.,r0
	add	$'0,r1
	movb	r1,-(r2)
	mov	r0,r1
	bne	pwrsl
1:
	mov	r2,r4
	sub	sp,r2
	sub	$14.,r2
	neg	r2
	mov	-2(r3),r3
	sub	r2,r3
	tst	14.(sp)
	beq	1f
	dec	r3
1:
	dec	r3
	bmi	1f
	mov	$' ,r0
	jsr	pc,2f
	br	1b
1:
	tst	14.(sp)
	beq	1f
	mov	$'-,r0
	jsr	pc,2f
1:
	dec	r2
	bmi	1f
	movb	(r4)+,r0
	jsr	pc,2f
	br	1b
1:
	add	$14.,sp
	jmp	cret
2:
	mov	r0,-(sp)		/ routine to call _pwrite with a char
	clr	-(sp)
	mov	$O_WRITC,-(sp)
	jsr	pc,_pwrite
	add	$6,sp
	rts	pc
/
/ Table of quotients and remainders of dividing integer high
/ words and zero low words by 10.  Table was generated using dc.
/
.data
pw_tab:
	0;	0
	6;  14631
	2;  31463
       10;  46314
	4;  63146
        0; 100000
        6; 114631
        2; 131463
       10; 146314
        4; 163146
.text
