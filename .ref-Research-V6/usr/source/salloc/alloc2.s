/
/	routine to read from disc to a buffer
/	wcing the buffer if necessary
/	mov	disc addr,r0
/	mov	buffer addr,r2
/	jsr	pc,getb
/	on return r0 = addr of byte in buffer
/
getb:
	mov	r3,-(sp)
	mov	r1,-(sp)
	mov	r0,-(sp)
	mov	r2,r3
	asr	r3
	mov	r3,r1
	ashc	$9.,r1
	bic	$777,r1
	add	$b1,r1
	tst	w1(r2)	/ w
	ble	1f

	jsr	pc,clean

1:	mov	(sp),r0
	bic	$777,r0		/get lowest multiple of 512.
	mov	r0,0f
	mov	r0,b1s(r2)	/set start
	mov	afi,r0
	sys	seek;0:..;0
	mov	r1,0f
	sys	read;0:..;512.

	mov	b1s(r2),b1e(r2)
	add	$512.,b1e(r2)	/ set end
	clr	w1(r2)	/clear w
	mov	(sp)+,r0
	sub	b1s(r2),r0
	add	r1,r0	/ set r0=byte addr in buffer
	mov	(sp)+,r1
	mov	(sp)+,r3
	rts	pc
/
/
/	routine to wc a buffer
/	mov	buffer addr,r2
/	mov	buffer addr+6,r1	beginning of buffer
/	jsr	pc,clean
/
clean:
	mov	r0,-(sp)
	mov	b1s(r2),0f
	mov	afout,r0
	sys	seek;0:..;0
	mov	r1,0f
	sys	write;0:..;512.

	clr	w1(r2)	/clear w
	mov	(sp)+,r0
	rts	pc
/
/	routine to get buffer addr of byte whose disc
/	addr is in r0 - also returns addr of write
/	flag for buffer in r2
/	mov	disc addr,r0
/	jsr	pc,bufchar
/	mov	(r0),r0	for read
/	inc	(r2)	for write must inc w
/	c-bit set if char not in either buffer
/
bufchar:
	mov	r1,-(sp)
	mov	r3,-(sp)
	clr	r3
1:
	mov	r3,r2
	asl	r2
	cmp	r0,b1s(r2)
	blo	2f
	cmp	r0,b1e(r2)
	bhis	2f
	sub	b1s(r2),r0
	mov	r3,r1
	ashc	$9.,r1
	bic	$777,r1
	add	r1,r0
	add	$b1,r0
	mov	(sp)+,r3
	mov	(sp)+,r1
	clc
	rts	pc
2:
	inc	r3
	cmp	r3,nbuf
	blt	1b
	mov	(sp)+,r3
	mov	(sp)+,r1
	sec
	rts	pc
/
/	routine to get a buffer
/	mov	disc addr,r0
/	jsr	pc,getbuf
/	mov	(r0),r0	(for read)
/	inc	(r2)	must inc w for w
/
getbuf:
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	$2,r3
	clr	r2
	mov	$1,r4
1:
	cmp	r4,nbuf
	bge	1f
	cmp	u1(r3),u1(r2)
	bhis	2f
	mov	r3,r2
2:
	inc	r4
	add	$2.,r3
	br	1b
1:
	mov	r2,r3
	jsr	pc,getb
	add	$stats+6.,r3
	inc	(r3)
	mov	(sp)+,r3
	mov	(sp)+,r4
	rts	pc
/
/	routine to look at next character from string
/	pointed to by r1;  character returned in r0
/	c-bit set if character not available (end of file)
/	r1 is preserved
/
/	mov	...,r1
/	jsr	pc,lookchar
/	movb	r0,...
/
lookchar:
	mov	r2,-(sp)
	jsr	pc,plausible
	cmp	w(r1),r(r1)	/W,R
	blos	noch
	mov	r(r1),r0
	jsr	pc,bufchar
	bec	2f
	jsr	pc,getbuf
/
2:
	inc	flag
	bne	2f
	jsr	pc,fixct
	br	1f
2:
	mov	flag,u1(r2)
1:
	mov	(sp)+,r2
	movb	(r0),r0
	tst	r0	/clears c-bit
	rts	pc
/
noch:
	mov	(sp)+,r2
	clr	r0
	sec
	rts	pc
/
/
/	routine to put a character into the string
/	pointed to by r1;  character in r0
/	r0 is preserved; r1 points to the string
/	after return and must be saved.
/
/	movb	ch,r0
/	mov	...,r1
/	jsr	pc,putchar
/	mov	r1,...
/
putchar:
	mov	r2,-(sp)
	mov	r1,-(sp)
	mov	r0,nchar
	jsr	pc,plausible
1:	cmp	w(r1),l(r1)	/W,L
	blt	3f
	mov	w(r1),r0
	inc	r0
	sub	a(r1),r0	/W-A+1
	jsr	pc,allocate
	mov	(sp),r0
	jsr	pc,copy
	jsr	pc,swap
	jsr	pc,release
	mov	(sp),r1
/
3:	mov	w(r1),r0
	jsr	pc,bufchar
	bec	2f
	jsr	pc,getbuf
2:	movb	nchar,(r0)
	mov	$1,w1(r2)

	mov	nchar,r0	/to preserve r0 for user
	inc	w(r1)
	inc	flag
	bne	2f
	jsr	pc,fixct
	br	1f
2:
	mov	flag,u1(r2)
1:
	mov	(sp)+,r1
	mov	(sp)+,r2
	rts	pc

/
/
fixct:
	mov	r1,-(sp)
	mov	r3,-(sp)
	mov	nbuf,r1
	mov	nbuf,flag
2:
	mov	r1,u1(r2)
	dec	r1
	bge	1f
	mov	(sp)+,r3
	mov	(sp)+,r1
	rts	pc
1:
	clr	r2
	mov	$2,r3
1:
	cmp	r3,nbuf2
	bge	2b
	cmp	u1(r3),u1(r2)
	blo	2f
	mov	r3,r2
2:
	add	$2,r3
	br	1b
