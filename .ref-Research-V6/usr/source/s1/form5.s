.globl b1
.globl hblk
.globl headers
.globl	initl
.globl asmem
.globl b1s
.globl b1e
.globl w1
.globl stats
.globl	lookchar
.globl	flush
.globl	fsfile
.globl	seekchar
.globl	backspace
.globl	alterchar
.globl	zero
.globl	getchar
.globl	putchar
.globl	copy
.globl	rewind
.globl	create
.globl	allocate
.globl	release
.globl	collect
.globl	w,r,a,l
.globl	getword
.globl	putword
.globl	backword
.globl	alterword
/
/
/	routine to read next character from string
/	pointer to by r1; character returned in r0
/	c-bit set if character not availiable (eof)
/
/	mov	...,r1
/	jsr	pc,getchar
/	movb	r0,...
/
getchar:
	jsr	pc,lookchar
	bes	1f
	inc	r(r1)
	tst	r0		/clears c-bit
1:	rts	pc
/
/
/	routine to read a string backwards
/	the read pointer is decremented before reading
/
/	mov	...,r1
/	jsr	pc,backspace
/	mov	r0,...
/
backspace:
	cmp	a(r1),r(r1)
	bhis	nochc
	dec	r(r1)
	jsr	pc,lookchar
	rts	pc
nochc:	clr	r0
	sec
	rts	pc
/
/
/	routine to put a word onto the string
/
/	mov	...,r1
/	mov	...,r0
/	jsr	pc,putword
/
putword:
	mov	r0,-(sp)
	sub	$hblk,r0
	jsr	pc,putchar
	swab	r0
	jsr	pc,putchar
	mov	(sp)+,r0
	rts	pc
/
/
/	routine to get a word from the string
/
/	mov	...,r1
/	jsr	pc,getword
/	mov	r0,...
/
getword:
	jsr	pc,lookchar
	bes	1f
	movb	r0,nchar
	inc	r(r1)
	jsr	pc,lookchar
	bes	1f
	movb	r0,nchar+1
	inc	r(r1)
	mov	nchar,r0
	add	$hblk,r0
1:	rts	pc
/
/
/	routine to alter the word pointed to by r(r1)
/	by replacing the word there with r0
/
/	mov	wd,r0
/	mov	...,r1
/	jsr	pc,alterword
/
alterword:
	mov	r0,-(sp)
	sub	$hblk,r0
	jsr	pc,alterchar
	swab	r0
	jsr	pc,alterchar
	mov	(sp)+,r0
	rts	pc
/
/
/	routine to get words backwards from string
/
/	mov	...,r1
/	jsr	pc,backword
/	mov	r0,...
/
backword:
	cmp	a(r1),r(r1)
	bhis	nochw
	dec	r(r1)
	jsr	pc,lookchar
	movb	r0,nchar+1
	cmp	a(r1),r(r1)
	bhis	nochw
	dec	r(r1)
	jsr	pc,lookchar
	movb	r0,nchar
	mov	nchar,r0
	add	$hblk,r0
	rts	pc
/
nochw:
	clr	r0
	sec
	rts	pc
/
/
/	routine to copy the contents of one string
/	to another.
/
/	mov	source,r0
/	mov	dest,r1
/	jsr	pc,copy
/	mov	r1,...
/
/	on return, r1 points to the new string and should
/	be saved.  r0 is preserved.
/
copy:
	inc	stats+12.
	mov	r0,-(sp)
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	w(r0),r2
	sub	a(r0),r2	/W-A (old)
	mov	l(r1),r3
	sub	a(r1),r3	/L-A (new)
	cmp	r2,r3
	blos	1f
	mov	r2,r0
	jsr	pc,allocate
	mov	4(sp),r0	/new
	jsr	pc,swap
	jsr	pc,release
	mov	r0,r1
	mov	0(sp),r0	/old
1:
	mov	a(r1),w(r1)	/rewind w pointer
	cmp	r2,$512.
	blos	copy1		/is a short string
/
	jsr	pc,flush
	jsr	pc,reset
/
	mov	a(r0),-(sp)
4:
	mov	(sp),0f
	mov	afi,r0
	sys	seek;0:.. ;0	/set input pointer
	cmp	r2,$512.
	blos	2f
	mov	$512.,r3	/# output this time
	mov	r3,0f
	mov	r3,3f
	add	r3,(sp)
	sub	r3,r2	/# left to output
	br	1f
2:
	mov	r2,0f
	mov	r2,3f
	mov	r2,r3
	clr	r2
1:
	mov	afi,r0
	sys	read;b1;0:..
	bes	bad
	cmp	r0,r3
	bne	bad
	mov	afout,r0
	mov	(r1),0f
	add	r3,(r1)
	sys	seek;0:.. ;0
	sys	write;b1;3:..
	bes	bad
	tst	r2
	bgt	4b
	tst	(sp)+
/
/	fix up read ptr of new string
/
copy2:
	mov	6(sp),r0	/restore r0
	mov	r(r0),r2
	sub	a(r0),r2
	add	a(r1),r2
	mov	r2,r(r1)
/
/	restore and return
/
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	mov	(sp)+,r0
	rts	pc
/
bad:	mov	$1,r0
	sys write;1f;2f-1f
	4
1:	<error on copy\n>
2:	.even
/
swap:
	mov	w(r1),-(sp)
	mov	w(r0),w(r1)
	mov	(sp),w(r0)
	mov	r(r1),(sp)

	mov	r(r0),r(r1)
	mov	(sp),r(r0)
	mov	a(r1),(sp)
	mov	a(r0),a(r1)
	mov	(sp),a(r0)
	mov	l(r1),(sp)
	mov	l(r0),l(r1)
	mov	(sp)+,l(r0)
	rts	pc
/
/	copy a short string
/
copy1:
	mov	r(r0),-(sp)
	mov	a(r0),r(r0)
	mov	nchar,-(sp)
	mov	r0,r2		/old
	mov	r1,r3		/new
1:
	mov	r2,r1
	jsr	pc,getchar
	bes	1f
	mov	r3,r1
	jsr	pc,putchar
	br	1b
1:
	mov	r2,r0
	mov	(sp)+,nchar
	mov	(sp)+,r(r0)
	mov	r3,r1
	br	copy2
/
/
/
/
/
/	routine to rewind read pointer of string
/	pointed to by r1
/
/	mov	...,r1
/	jsr	pc,rewind
/
rewind:
	mov	a(r1),r(r1)
	rts	pc
/
/
/	routine to rewind write pointer of string
/	pointed to by r1
/
/	mov	...,r1
/	jsr	pc,create
/
create:
	mov	a(r1),w(r1)
	mov	a(r1),r(r1)
	rts	pc
/
/
/	routine to zero a string
/
/	mov	...,r1
/	jsr	pc,zero
/
zero:
	mov	r0,-(sp)
	.if testing
	jsr	pc,preposterous
	.endif
	mov	a(r1),w(r1)
	clrb	r0
1:	cmp	w(r1),l(r1)
	bhis	1f
	jsr	pc,putchar
	br	1b
1:	mov	a(r1),w(r1)
	mov	(sp)+,r0
	rts	pc
/
/
/
/	routine to move the read pointer of a string to the
/	relative position indicated by r0.  the string is
/	extended if necessary - there is no error return.
/
/	mov	position,r0
/	mov	...,r1
/	jsr	pc,seekchar
/
seekchar:
	mov	r1,-(sp)
	mov	r0,-(sp)
	.if testing
	jsr	pc,preposterous
	.endif
	inc	stats+10.
1:
	mov	(sp),r0
	add	a(r1),r0
	cmp	r0,l(r1)
	bhi	3f
	mov	r0,r(r1)
	cmp	r0,w(r1)
	blo	1f
	mov	r0,w(r1)
	br	1f
3:
	mov	(sp),r0
	jsr	pc,allocate
	mov	2(sp),r0
	jsr	pc,copy
	jsr	pc,swap
	jsr	pc,release
	mov	2(sp),r1
	br	1b
1:
	mov	(sp)+,r0
	mov	(sp)+,r1
	rts	pc
/
/
/	routine to move read pointer of string to end of string
/
/	mov	...,r1
/	jsr	pc,fsfile
/
fsfile:
	mov	r0,-(sp)
	.if testing
	jsr	pc,preposterous
	.endif
	inc	stats+10.
	mov	w(r1),r(r1)
	mov	(sp)+,r0
	rts	pc
/
/
/	routine to place the character in r0 at the current
/	position of the read pointer - the read pointer
/	is not moved.
/
/	movb	ch,r0
/	mov	...,r1
/	jsr	pc,alterchar
/	mov	r1,...
/
alterchar:
	mov	r2,-(sp)
	mov	r1,-(sp)
	mov	r0,nchar
	.if testing
	jsr	pc,preposterous
	.endif
	inc	stats+8.
1:	cmp	r(r1),l(r1)	/W,L
	blo	3f
	mov	l(r1),r0
	inc	r0
	sub	a(r1),r0	/W-A+1
	jsr	pc,allocate
	mov	(sp),r0
	jsr	pc,copy
	jsr	pc,swap
	jsr	pc,release
	mov	(sp),r1
3:
	mov	r(r1),r0
	jsr	pc,bufchar
	bec	2f
	jsr	pc,getbuf

2:	movb	nchar,(r0)
	mov	$1,w1(r2)
	mov	nchar,r0	/to preserve r0 for user
	inc	r(r1)
	cmp	r(r1),w(r1)
	blos	3f
	mov	r(r1),w(r1)
3:
	mov	(sp)+,r1
	mov	(sp)+,r2
	rts	pc
/
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
	inc	stats+6.
	.if testing
	jsr	pc,preposterous
	.endif
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
	.if testing
	jsr	pc,preposterous
	.endif
	inc	stats+8.
1:	cmp	w(r1),l(r1)	/W,L
	blo	3f
	mov	w(r1),r0
	inc	r0
	sub	a(r1),r0	/W-A+1
	jsr	pc,allocate
	mov	(sp),r0
	jsr	pc,copy
	jsr	pc,swap
	jsr	pc,release
	mov	(sp),r1
3:
	mov	w(r1),r0
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
/	routine to flush contents of all buffers.
/
/	jsr	pc,flush
/
flush:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	clr	r3
1:
	cmp	r3,$numb
	bhis	1f
	mov	r3,r2
	asl	r2
	tst	w1(r2)
	ble	2f
	mov	r3,r1
	ashc	$9.,r1
	bic	$777,r1
	add	$b1,r1
	jsr	pc,clean
2:
	inc	r3
	br	1b
1:
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	pc
/
/
reset:
	mov	r3,-(sp)
	mov	r2,-(sp)
	clr	r3
1:
	cmp	r3,$numb
	bge	1f
	mov	r3,r2
	asl	r2
	mov	$-1.,w1(r2)
	clr	b1s(r2)
	clr	b1e(r2)
	clr	u1(r2)
	inc	r3
	br	1b
1:
	clr	flag
	mov	(sp)+,r2
	mov	(sp)+,r3
	rts	pc
/
/
/	routine to read from disc to a buffer
/	wcing the buffer if necessary
/
/	mov	disc addr,r0
/	mov	buffer addr,r2
/	jsr	pc,getb
/
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
	clr	w1(r2)		/clear w
	mov	(sp)+,r0
	sub	b1s(r2),r0
	add	r1,r0		/ set r0=byte addr in buffer
	mov	(sp)+,r1
	mov	(sp)+,r3
	rts	pc
/
/
/	routine to wc a buffer
/
/	mov	buffer addr,r2
/	mov	buffer addr+6,r1	beginning of buffer
/	jsr	pc,clean
/
clean:
	inc	stats+24.
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
/
/	routine to get buffer addr of byte whose disc
/	addr is in r0 - also returns addr of write
/	flag for buffer in r2
/
/	mov	disc addr,r0
/	jsr	pc,bufchar
/	mov	(r0),r0	for read
/	inc	(r2)	for write must inc w
/
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
	cmp	r3,$numb
	blt	1b
	mov	(sp)+,r3
	mov	(sp)+,r1
	sec
	rts	pc
/
/
/	routine to get a buffer
/
/	mov	disc addr,r0
/	jsr	pc,getbuf
/	mov	(r0),r0		(for read)
/	inc	(r2)		must inc w for w
/
getbuf:
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	$2,r3
	clr	r2
	mov	$1,r4
1:
	cmp	r4,$numb
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
	add	$stats+14.,r3
	inc	(r3)
	mov	(sp)+,r3
	mov	(sp)+,r4
	rts	pc
/
/
/	this routine renumbers the time used cell u1(r2)
/	of the buffers when the clock overflows
/
fixct:
	mov	r1,-(sp)
	mov	r3,-(sp)
	mov	$numb,r1
	mov	$numb,flag
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
	cmp	r3,$numb2
	bge	2b
	cmp	u1(r3),u1(r2)
	blo	2f
	mov	r3,r2
2:
	add	$2,r3
	br	1b
