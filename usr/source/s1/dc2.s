/
/
/	routine to add the two centennial numbers
/	pointed to by r2 and r3.
/	a pointer to the result is returned in r1
/	r2 and r3 are preserved
/
/	mov	ptr1,r2
/	mov	ptr2,r3
/	jsr	pc,add3
/	mov	r1,...
/
add3:	mov	r0,-(sp)
	mov	r4,-(sp)
	mov	r5,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
/
/	allocate a new string whose length is
/	the max of the two addends.
/
	mov	w(r2),r0
	sub	a(r2),r0
	mov	w(r3),r4
	sub	a(r3),r4
	cmp	r0,r4
	bgt	1f
	mov	r4,r0
1:	mov	r0,r4
	jsr	pc,allocate
	mov	r1,-(sp)
/
/	get everything ready
/
	mov	2(sp),r1
	jsr	pc,rewind
	mov	4(sp),r1
	jsr	pc,rewind
	clr	carry
/
/	now add them
/
2:	dec	r4
	blt	3f
	mov	2(sp),r1	/r2
	jsr	pc,getchar
	mov	r0,r5
	mov	4(sp),r1	/r3
	jsr	pc,getchar
	add	r5,r0
	add	carry,r0
	clr	carry
	cmp	r0,$100.
	blt	1f
	sub	$100.,r0
	mov	$1,carry
1:
	tstb	r0
	bpl	1f
	add	$100.,r0
	mov	$-1,carry
1:	mov	(sp),r1		/r1
	jsr	pc,putchar
	br	2b
/
/	perhaps there is an extra digit
/
3:	mov	carry,r0
	beq	2f
	mov	(sp),r1		/r1
	jsr	pc,putchar
/
/	strip leading zeros
/
2:
	jsr	pc,fsfile
2:	jsr	pc,backspace
	bes	2f
	beq	2b
	inc	r(r1)
2:	mov	r(r1),w(r1)
/
/	strip leading 99's
/
	jsr	pc,fsfile
	jsr	pc,backspace
	cmpb	r0,$-1
	bne	1f
2:
	jsr	pc,backspace
	bes	2f
	cmpb	r0,$99.
	beq	2b
	jsr	pc,getchar
2:
	mov	$-1,r0
	jsr	pc,alterchar
	mov	r(r1),w(r1)
/
/	restore and return
/
1:
	mov	(sp)+,r1
	mov	(sp)+,r2
	mov	(sp)+,r3
	mov	(sp)+,r5
	mov	(sp)+,r4
	mov	(sp)+,r0
	rts	pc
/
.bss
carry:	.=.+2
.text
/
/
/	routine to change the sign of the centennial number
/	pointed to by r1.
/	negative numbers are stored in 100's complement form with
/	-1 as the high order digit; the second digit is not 99.
/
/	mov	...,r1
/	jsr	pc,chsign
/
chsign:
	mov	r1,-(sp)
	mov	r0,-(sp)
	jsr	pc,rewind
	clr	chcarry
/
1:
	jsr	pc,lookchar
	bes	1f
	negb	r0
	sub	chcarry,r0
	mov	$1,chcarry
	add	$100.,r0
	cmpb	$100.,r0
	bgt	2f
	sub	$100.,r0
	clr	chcarry
2:
	jsr	pc,alterchar
	br	1b
/
1:
	clr	r0
	sub	chcarry,r0
	beq	2f
	jsr	pc,putchar
	jsr	pc,fsfile
	jsr	pc,backspace
	jsr	pc,backspace
	cmp	r0,$99.
	bne	1f
	mov	r(r1),w(r1)
	mov	$-1,r0
	jsr	pc,putchar
	br	1f
/
2:
	jsr	pc,fsfile
	jsr	pc,backspace
	bne	1f
	mov	r(r1),w(r1)
/
1:
	mov	(sp)+,r0
	mov	(sp)+,r1
	rts	pc
/
.bss
chcarry: .=.+2
.text
/
/
/
/
/	routine to multiply the two centennial numbers
/	pointed to by r2 and r3.
/	a pointer to the result is returned in r1
/	r2 and r3 are preserved
/
/	mov	ptr1,r2
/	mov	ptr2,r3
/	jsr	pc,mul3
/	mov	r1,...
/
/	save registers and make space for temps
/
mul3:
	mov	r5,-(sp)
	mov	r3,-(sp)	/arg2
	mov	r2,-(sp)	/arg1
	mov	r0,-(sp)
	tst	-(sp)		/result
	tst	-(sp)		/arg1
	tst	-(sp)		/arg2
	tst	-(sp)		/carry
/
/	compute sign of result and make args positive
/
	clr	outsign
	mov	r2,r1
	jsr	pc,fsfile
	jsr	pc,backspace
	bmi	2f
	mov	r2,4(sp)	/arg1
	br	1f
2:
	jsr	pc,length
	jsr	pc,allocate
	mov	r1,4(sp)
	mov r2,r0
	jsr	pc,move
	jsr	pc,chsign
	com	outsign
1:
	mov	r3,r1
	jsr	pc,fsfile
	jsr	pc,backspace
	bmi	2f
	mov	r3,2(sp)	/arg2
	br	1f
2:
	mov	r3,r1
	jsr	pc,length
	jsr	pc,allocate
	mov	r1,2(sp)
	mov	r3,r0
	jsr	pc,move
	jsr	pc,chsign
	com	outsign
1:
/
/	compute the length of the result and
/	allocate space for it
/
	mov	w(r2),r0
	sub	a(r2),r0
	add	w(r3),r0
	sub	a(r3),r0
	jsr	pc,allocate
	jsr	pc,zero
	mov	r1,6(sp)	/result
	clr	offset
	mov	2(sp),r1	/arg2
	jsr	pc,rewind
/
/	work on next digit of arg2, starting over on arg1
/
1:	mov	4(sp),r1	/arg1
	jsr	pc,rewind
	mov	2(sp),r1	/arg2
	jsr	pc,getchar
	bes	3f
	mov	r0,r2
	mov	6(sp),r1	/result
	jsr	pc,rewind
	add	offset,r(r1)
	clr	0(sp)		/carry
/
/	work on next digit of arg3
/	form the product of the two digits,
/	add to what is already there and add in old carry
/	to generate new dit and new carry.
/
2:	mov	4(sp),r1	/arg1
	jsr	pc,getchar
	bes	2f
	mov	r0,r3
	mpy	r2,r3
	add	(sp),r3		/carry
	mov	6(sp),r1	/result
	jsr	pc,lookchar
	add	r0,r3
	mov	r3,r1
	clr	r0
	dvd	$100.,r0
	mov	r0,(sp)		/carry
	mov	r1,r0
	mov	6(sp),r1	/result
	jsr	pc,alterchar
	br	2b
/
2:
	inc	offset
	tst	(sp)		/carry
	beq	1b
	mov	6(sp),r1	/result
	jsr	pc,lookchar
	add	(sp),r0		/carry
	jsr	pc,alterchar
	br	1b
/
3:
/
/	change sign of result if necessary
/
	tst	outsign
	bpl	1f
	mov	6(sp),r1	/result
	jsr	pc,chsign
/
/	release dregs if necessary
/
1:
	cmp	2(sp),14(sp)
	beq	1f
	mov	2(sp),r1
	jsr	pc,release
1:
	cmp	4(sp),12(sp)
	beq	1f
	mov	4(sp),r1
	jsr	pc,release
1:
/
/	restore registers and return
/
	tst	(sp)+
	tst	(sp)+
	tst	(sp)+
	mov	(sp)+,r1
	mov	(sp)+,r0
	mov	(sp)+,r2
	mov	(sp)+,r3
	mov	(sp)+,r5
	rts	pc
/
.bss
outsign: .=.+2
offset:	.=.+2
k:	.=.+2
kptr:	.=.+2
.text
/
sqrt:
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
	mov	r0,-(sp)
/
/	check for zero or negative
/
	mov	w(r1),r2
	sub	a(r1),r2
/
/	look at the top one or two digits
/
	mov	r1,r3
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,r4
	bit	$1,r2
	bne	2f
	mov	r4,r1
	mul	$100.,r1
	mov	r1,r4
	mov	r3,r1
	jsr	pc,backspace
	add	r0,r4
2:
/
/	allocate space for result
/
	inc	r2
	asr	r2
	mov	r2,r0
	jsr	pc,allocate
	jsr	pc,zero
	mov	r2,r0
	jsr	pc,seekchar
	mov	r1,r2
/
/	get high order digit of arg and square root it
/
	mov	$1,r0
2:	sub	r0,r4
	blt	2f
	add	$2,r0
	br	2b
2:	inc	r0
	asr	r0
	mov	r0,r4
	mov	r2,r1
	jsr	pc,fsfile
	jsr	pc,backspace
	cmp	r4,$100.
	blt	1f
	sub	$100.,r4
	mov	r4,r0
	jsr	pc,alterchar
	mov	$1,r0
	jsr	pc,putchar
	br	2f
1:
	mov	r4,r0
	jsr	pc,alterchar
2:
	mov	r1,-(sp)
	mov	r3,-(sp)
/
/	get successive approx. from Newton
/
1:	mov	(sp),r3		/arg
	mov	2(sp),r2	/approx
	jsr	pc,div3
	mov	r1,r3
	jsr	pc,add3
	mov	r1,-(sp)
	mov	r3,r1
	jsr	pc,release
	mov	r4,r1
	jsr	pc,release
	mov	(sp)+,r1
	mov	sqtemp,r2
	mov	r1,r3
	jsr	pc,div3
	mov	r1,-(sp)
	mov	r3,r1
	jsr	pc,release
	mov	r4,r1
	jsr	pc,release
	mov	(sp)+,r3
	mov	2(sp),r1
	jsr	pc,length
	jsr	pc,allocate
	mov	2(sp),r0
	jsr	pc,move
	jsr	pc,chsign
	mov	r1,r2
	jsr	pc,add3
	jsr	pc,fsfile
	jsr	pc,backspace
	jsr	pc,release
	mov	r2,r1
	jsr	pc,release
	tst	r0
	bpl	2f
/
/	loop if new < old
	mov	2(sp),r1
	jsr	pc,release
	mov	r3,2(sp)
	br	1b
/
2:
	mov	r3,r1
	jsr	pc,release
	mov	(sp)+,r1
	jsr	pc,release
	mov	(sp)+,r1
	mov	(sp)+,r0
	mov	(sp)+,r2
	mov	(sp)+,r3
	mov	(sp)+,r4
	rts	pc
