/	scan
/	r1 = pointer to input header
/	r0 = pointer to output header
/	switch = 0 real output
/		not 0  output to r0
scan:
	mov	r0,-(sp)
	mov	r1,-(sp)

1:
	mov	(sp),r1
	jsr	pc,getchar
	bes	5f
	cmpb	r0,$'\\
	beq	3f
	cmpb	r0,$'\[
	beq	4f
	cmpb	r0,$'\{
	beq	4f
	cmpb	r0,$'\]
	beq	1f
	cmpb	r0,$'\}
	beq	7f
	cmpb	r0,$'\n
	bne	6f
	jsr	pc,lookchar
	bes	5f
	movb	$'\n,r0
6:
	tst	switch
	beq	2f
	mov	2(sp),r1
	jsr	pc,putchar
	br	1b
2:
	jsr	r5,putc
	br	1b
3:
	jsr	pc,getchar
	br	6b
/	have [ or {
4:
	mov	r(r1),-(sp)
	jsr	pc,getchar
	bes	1f
	cmpb	r0,$'0
	blt	4f
	cmpb	r0,$'9
	bgt	4f
	jsr	pc,getchar
	cmpb	r0,$'\]
	beq	3f
	cmpb	r0,$'\}
	beq	3f

4:
	mov	(sp)+,r(r1)
	mov	2(sp),r0
	jsr	pc,esub
	mov	r1,(sp)
	mov	r0,2(sp)
	br	1b

/	have arg
3:
	tst	switch
	beq	2f
	mov	opointer,r1
	mov	4(sp),r0
	jsr	pc,putword
	mov	2(sp),r1
2:
	mov	(sp)+,r(r1)
	jsr	pc,getchar
	inc	r(r1)	/skip ]
	jsr	pc,earg
	br	1b

5:
	tst	argf
	bne	1f
	mov	ipointer,r1
	jsr	pc,pop
	bes	1f
	mov	r0,(sp)
	br	1b
7:
	inc	curlyf
1:
	mov	(sp)+,r1
	mov	(sp)+,r0
	rts	pc
