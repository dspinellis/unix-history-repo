/
/
/	routine to divide the two centennial numbers pointed
/	to by r2 (the divisor) and r3 (the dividend).
/	A pointer to the result is returned in r1.  All other
/	registers are preserved.  If the divisor is zero, zero
/	is returned and the carry bit is set.
/	Remainder is returned in r4 and has the sign
/	of the dividend.
/
/
/	mov	divisor,r2
/	mov	dividend,r3
/	jsr	pc,div3
/	mov	r1,result
/	mov	r4,remainder
/
/
div3:
	mov	r5,-(sp)
	mov	r3,-(sp)	/dividend
	mov	r2,-(sp)	/divisor
	mov	r0,-(sp)
	tst	-(sp)	/result
/
/	allocate space for result; allocate temps if necessary
/
	clr	r0
	jsr	pc,allocate
	mov	r1,0(sp)	/result
/
/
/	check for divisor zero
/
	mov	4(sp),r2	/divisor
	mov	w(r2),r0
	sub	a(r2),r0
	bne	1f
	jmp	eh
1:
/
/	compute sign of result and make arguments positive
/
	clr	divsign
	mov	r2,r1
	jsr	pc,length
	jsr	pc,allocate
	mov	r1,divisor
	mov	r2,r0
	jsr	pc,move
	jsr	pc,fsfile
	jsr	pc,backspace
	bpl	1f
2:
	jsr	pc,chsign
	mov	r1,divisor
	com	divsign
1:
	clr	remsign
	mov	r3,r1
	jsr	pc,length
	jsr	pc,allocate
	mov	r1,dividend
	mov	r3,r0
	jsr	pc,move
	jsr	pc,fsfile
	jsr	pc,backspace
	bpl	1f
2:
	jsr	pc,chsign
	mov	r1,dividend
	com	divsign
	com	remsign
1:
/
/
/	find out how many digits in the quotient result
/
1:
	mov	dividend,r2
	mov	divisor,r3
	mov	w(r2),r0
	sub	a(r2),r0
	add	a(r3),r0
	sub	w(r3),r0
	jlo	bugout
	mov	r0,divoffset
	mov	0(sp),r1	/result
	inc	r0
	jsr	pc,seekchar
	clr	r0
	mov	dividend,r1
	jsr	pc,putchar
/
/	load r5 with binary divisor for finding
/	trial quotient digits. If leading digit of
/	divisor is <10, it is scaled
/
	clr	magic
	mov	divisor,r1
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,r5
	cmp	r5,$10.
	bge	2f
	inc	magic
2:
	mpy	$100.,r5
	jsr	pc,backspace
	add	r0,r5
	tst	magic
	beq	2f
	mov	r5,r4
	mpy	$100.,r4
	jsr	pc,backspace
	add	r0,r5
	adc	r4
	asl	r5
	rol	r4
	dvd	$25.,r4
	mov	r4,r5
2:
/
/	compute trial quotient digit
/
1:
	mov	dividend,r1
	jsr	pc,fsfile
	jsr	pc,backspace
	bec 9f; 4; 9:
	mov	r0,r3
	mpy	$100.,r3
	mov	r3,r2
	jsr	pc,backspace
	add	r0,r2
	mpy	$100.,r2
	jsr	pc,backspace
	add	r0,r3
	adc	r2
	tst	divoffset
	bne	2f
	add	$1,r3
	adc	r2
2:
/
	tst	magic
	beq	3f
	ashc	$3,r2
3:
	mov	r5,r0
	tst	divoffset
	beq	2f
	inc	r0
2:
	dvd	r0,r2
	mov	r2,trial
/
/
/	multiply divisor by trial digit
/
	mov	divisor,r1
	jsr	pc,rewind
	jsr	pc,length
	inc	r0
	mov	divxyz,r1
	jsr	pc,rewind
	clr	-(sp)
2:
	mov	divisor,r1
	jsr	pc,getchar
	bes	2f
	mov	r0,r3
	mpy	trial,r3
	add	(sp),r3		/carry
	clr	r2
	dvd	$100.,r2
	mov	r2,(sp)		/carry
	mov	r3,r0
	mov	divxyz,r1
	jsr	pc,alterchar
	br	2b
2:
	mov	divxyz,r1
	mov	(sp)+,r0
	jsr	pc,alterchar
3:
/
/	and subtract from dividend
/
	jsr	pc,rewind
	mov	divoffset,r0
	mov	dividend,r1
	jsr	pc,seekchar
	clr	-(sp)
/
2:	mov	dividend,r1
	jsr	pc,lookchar
	bes	2f
	mov	r0,r2
/
	mov	divxyz,r1
	jsr	pc,getchar
	sub	r0,r2
	sub	(sp),r2
	clr	(sp)
	mov	r2,r0
	bpl	3f
	add	$100.,r0
	mov	$1.,(sp)
3:	mov	dividend,r1
	jsr	pc,alterchar
	br	2b
/
/	put away the quotient digit
/
2:
	mov	(sp)+,divcarry
	mov	0(sp),r1	/result
	jsr	pc,backspace
	mov	trial,r0
	jsr	pc,alterchar
	jsr	pc,backspace
/
/	and go for another digit
/
	dec	divoffset
	bmi	1f
	mov	dividend,r1
	dec	w(r1)
	cmp	w(r1),a(r1)
	bhis 9f; 4; 9:
	jmp	1b
/
/	fix up the result
/
1:
	tst	divcarry
	beq	1f
	mov	trial,r0
	dec	r0
	jsr	pc,alterchar
	mov	dividend,r1
	mov	$-1,r0
	jsr	pc,alterchar
	mov	divisor,r2
	mov	dividend,r3
	jsr	pc,add3
	mov	r1,-(sp)
	mov	r3,r1
	jsr	pc,release
	mov	(sp)+,dividend
1:
	mov	0(sp),r1	/result
	jsr	pc,rewind
	clr	divcarry
1:
	jsr	pc,lookchar
	bes	1f
	bic	$!377,r0
	add	divcarry,r0
	clr	divcarry
	cmp	r0,$100.
	blt	2f
	sub	$100.,r0
	inc	divcarry
2:	jsr	pc,alterchar
	br	1b
/
1:
	tst	divcarry
	beq	1f
	mov	$1.,r0
	jsr	pc,alterchar
1:
	jsr	pc,fsfile
1:
	jsr	pc,backspace
	bes	1f
	bne	1f
	mov	r(r1),w(r1)
	br	1b
1:
/
/	change sign of result if necessary
/
	tst	divsign
	bpl	1f
	jsr	pc,chsign
1:
	mov	dividend,r1
	jsr	pc,fsfile
1:
	jsr	pc,backspace
	bes	1f
	bne	1f
	mov	r(r1),w(r1)
	br	1b
1:
bugout:
	tst	remsign
	bpl	1f
	mov	dividend,r1
	jsr	pc,chsign
/
/	clean up junk, restore registers, and return
/
1:
	mov	divisor,r1
	jsr	pc,release
	mov	(sp)+,r1
	mov	(sp)+,r0
	mov	(sp)+,r2
	mov	(sp)+,r3
	mov	dividend,r4
	mov	(sp)+,r5
	clc
	rts	pc
/
/
/
/
.bss
divisor: .=.+2
dividend: .=.+2
divxyz:	.=.+2
divoffset:.=.+2
divcarry: .=.+2
divsign: .=.+2
trial:	.=.+2
remsign: .=.+2
magic:	.=.+2
.text
/
/
/
/	routine to exponentiate the two centennial numbers
/	pointed to by r2 (the base) and r3 (the exponent).
/	A pointer to the result is returned in r1.
/
/	mov	base,r2
/	mov	exp,r3
/	jsr	pc,exp3
/	mov	r1,...
/
/
/	save registers
/
exp3:
	mov	r3,-(sp)	/exponent
	mov	r2,-(sp)	/base
	mov	r0,-(sp)
/
/
1:
	mov	$1,r0
	jsr	pc,allocate
	mov	r1,-(sp)	/accumulated result
	mov	$1,r0
	jsr	pc,putchar
/
	mov	r2,r1
	jsr	pc,length
	jsr	pc,allocate
	mov	r1,-(sp)	/powers of the base
	mov	r2,r0
	jsr	pc,move
/
	mov	r3,r1
	jsr	pc,length
	jsr	pc,allocate
	mov	r1,-(sp)	/exponent
	mov	r3,r0
	jsr	pc,move
	jsr	pc,fsfile
	clr	exptemp
	jsr	pc,backspace
	bpl	1f
	inc	exptemp
	jsr	pc,chsign
/
1:
	mov	0(sp),r1
	jsr	pc,length
	beq	1f
	mov	sqtemp,r2
	mov	0(sp),r3
	jsr	pc,div3
	mov	r1,0(sp)
	mov	r3,r1
	jsr	pc,release
	mov	r4,r1
	jsr	pc,length
	jsr	pc,release
	tst	r0
	beq	2f
/
/
/
	mov	2(sp),r2
	mov	4(sp),r3
	jsr	pc,mul3
	mov	r1,4(sp)
	mov	r3,r1
	jsr	pc,release
2:
	mov	2(sp),r3
	mov	r3,r1
	jsr	pc,length
	jsr	pc,allocate
	mov	r1,r2
	mov	r3,r0
	jsr	pc,move
	jsr	pc,mul3
	mov	r1,2(sp)
	mov	r3,r1
	jsr	pc,release
	mov	r2,r1
	jsr	pc,release
	br	1b
1:
	tst	exptemp
	beq	1f
	mov	10(sp),r1
	jsr	pc,length
	bne	2f
	jmp	eh
2:
	cmp	r0,$1
	blos	2f
	mov	4(sp),r1
	jsr	pc,create
	br	1f
2:
	jsr	pc,rewind
	jsr	pc,getchar
	cmp	r0,$1
	bgt	2f
	mov	4(sp),r1
	jsr	pc,create
	jsr	pc,putchar
	br	1f
2:
	mov	4(sp),r1
	jsr	pc,create
1:
	mov	(sp)+,r1
	jsr	pc,release
	mov	(sp)+,r1
	jsr	pc,release
	mov	(sp)+,r1
/
	mov	(sp)+,r0
	mov	(sp)+,r2
	mov	(sp)+,r3
	rts	pc
/
.bss
exptemp: .=.+2
.text
/
