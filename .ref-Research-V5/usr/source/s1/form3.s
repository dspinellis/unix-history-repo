/	esub
/	r1 = pointer to input string
/
esub:
	mov	r1,-(sp)
	tst	switch
	beq	1f
	mov	opointer,r1
	jsr	pc,putword
1:
	tst	iflag
	beq	1f
	mov	$2,tflag
	jmp	interrupt
1:
	inc	switch
	mov	$10.,r0
	jsr	pc,allocate
	mov	r1,r0
	mov	opointer,r1
	jsr	pc,putword
	mov	(sp)+,r1
	jsr	pc,scan

	tst	iflag
	beq	1f
	mov	$3,tflag
	jmp	interrupt
1:
	mov	r2,-(sp)
	mov	r1,r0
	mov	ipointer,r1
	jsr	pc,putword
	mov	opointer,r1
	jsr	pc,pop
	bec	9f; 4; 9:
	mov	r0,-(sp)	/last output
	mov	asmem,r1
	jsr	pc,getsp
	bes	2f
5:
	clr	curlyf
	mov	(sp),r1
	mov	r0,r2
	jsr	pc,release
6:
	mov	opointer,r1
	jsr	pc,pop
	bec	4f
	clr	switch
4:	mov	r2,r1
	jsr	pc,rewind
	tst	(sp)+
	mov	(sp)+,r2
	rts	pc


/	not in memory
2:
	mov	curly,r1
	jsr	pc,getsp
	bes	2f
	br	5b
2:
	mov	(sp),r1
	jsr	pc,rewind
	mov	$'\[,r0
1:
	jsr	r5,tfoput
	jsr	pc,getchar
	bec	1b
	mov	$'\],r0
	jsr	r5,tfoput
	mov	$10.,r0
	jsr	pc,allocate
	mov	r1,r2
	mov	(sp),r0
	tst	curlyf
	beq	1f
	clr	curlyf
	mov	curly,r1
	jsr	pc,addentry
	br	2f
1:
	mov	asmem,r1
	jsr	pc,whead
	jsr	pc,addentry
2:
	mov	r2,r1
	jsr	pc,accept
	br	6b
/
/
/	earg
/	r0 = number in ascii
/
earg:
	tst	iflag
	beq	1f
	mov	$4,tflag
	jmp	interrupt
1:
	mov	r1,-(sp)
	inc	argf
	sub	$'0,r0
	asl	r0
	mov	arg(r0),r1
	beq	5f
4:
	mov	(sp),r0
	mov	r1,(sp)
	mov	ipointer,r1
	jsr	pc,putword
	mov	opointer,r1
	jsr	pc,pop
	bec	3f
	clr	switch
3:
	mov	(sp)+,r1
	jsr	pc,rewind
	jsr	pc,scan
	mov	r0,-(sp)
	mov	ipointer,r1
	jsr	pc,pop
	bec	9f; 4; 9:
	mov	r0,r1
	clr	argf
	clr	curlyf
	mov	(sp)+,r0
	rts	pc
5:
	mov	r0,-(sp)
	mov	$'\[,r0
	jsr	r5,tfoput
	mov	(sp),r0
	asr	r0
	add	$'0,r0
	jsr	r5,tfoput
	mov	$'\],r0
	jsr	r5,tfoput
	mov	$10.,r0
	jsr	pc,allocate
	mov	(sp)+,r0
	mov	r1,arg(r0)
	jsr	pc,accept

	br	4b
