rti = 2
/	accept
/	r1 = header of where to put it
/
accept:
	mov	r0,-(sp)
	movb	$':,r0
	jsr	r5,tfoput
	movb	$' ,r0
	jsr	r5,tfoput
2:
	jsr	r5,tfiget
	cmpb	r0,$'\n
	beq	1f
	jsr	pc,putchar
	br	2b
1:
	jsr	pc,putchar
	jsr	r5,tfiget
	cmpb	r0,$'\n
	beq	1f
	jsr	pc,putchar
	br	2b
1:
	mov	(sp)+,r0
	rts	pc
/
/
/	routine to get the last word from the string
/	r1=header addr
/
pop:
	jsr	pc,fsfile
	jsr	pc,backword
	bes	1f
	sub	$2,w(r1)
1:
	rts	pc
/
/
/	routine to add an entry to assoc mem or curly
/	r1 = header
/	r0 = header of string name
/	r2 = header of string
/
addentry:
	jsr	pc,putword
	mov	r2,r0
	jsr	pc,putword
	rts	pc
/
/
/	routine to find the string pointed to by r0
/	in either memory or curly
/	r1 = where to look
/
/	returns error set if string not there
/	returns r1 = header of string if there
/
getsp:
	mov	r0,-(sp)
	mov	r1,-(sp)
	jsr	pc,rewind	/memory
3:
	mov	2(sp),r1
	jsr	pc,rewind	/output
	mov	(sp),r1
	jsr	pc,getword
	bes	1f
	mov	r0,r1
	mov	r0,-(sp)
	jsr	pc,rewind	/string
2:
	mov	(sp),r1
	jsr	pc,getchar
	bes	2f
	movb	r0,r2
	mov	4(sp),r1
	jsr	pc,getchar
	bes	4f
	cmpb	r0,r2
	beq	2b
4:
	tst	(sp)+
4:
	mov	(sp),r1
	add	$2,r(r1)
	br	3b
2:
	tst	(sp)+
	mov	2(sp),r1
	cmp	r(r1),w(r1)
	bne	4b
	mov	(sp)+,r1
	jsr	pc,getword
	tst	(sp)+
	rts	pc
1:
	mov	(sp)+,r1
	mov	(sp)+,r0
	sec
	rts	pc
/
/
/	tfoput
/
/	jsr	r5,tfoput
/
tfoput:
	mov	r0,ch
	mov	$1,r0
	sys	write; ch; 1
	rts	r5
/
/
/	tfiget
/
/	jsr	r5,tfiget
/
tfiget:
	clr	r0
	sys	read; ch; 1
	bes	1f
	tst	r0
	beq	1f
	movb	ch,r0
	rts	r5
1:
	jsr	r5,flushb
	tst	argf
	beq	1f
	mov	opointer,r1
	jsr	pc,pop
1:
	mov	$5,tflag
	jmp	interrupt
/
/	putc
/	jsr	r5,putc
/
putc:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	$rfo+2,r1
	mov	r1,r2
	add	(r1),r2
	movb	r0,2(r2)
	inc	(r1)
	cmp	(r1),$512.
	bge	1f
	mov	(sp)+,r2
	mov	(sp)+,r1
	rts	r5
1:
	mov	(sp)+,r2
	mov	(sp)+,r1
/
/
/	flush output buffer to output file.
/
flushb:
	mov	r1,-(sp)
	mov	r0,-(sp)
	mov	$rfo,r1
	mov	(r1)+,r0
	mov	(r1),0f+2
	beq	1f
	clr	(r1)+
	mov	r1,0f
	sys	write; 0:..;..
1:
	mov	(sp)+,r0
	mov	(sp)+,r1
	rts	r5
relarg:
	mov	$arg,r2
1:
	mov	(r2)+,r1
	beq	2f
	jsr	pc,release
	br	1b
2:
	cmp	r2,$arge
	blt	1b
	rts	pc
/
relcurl:
	mov	curly,r1
	jsr	pc,rewind
1:
	jsr	pc,getword
	bes	1f
	mov	r0,r1
	jsr	pc,release
	mov	curly,r1
	br	1b
1:
	jsr	pc,release
	rts	pc
/
int:
	inc	iflag
	rti
/
interrupt:
	jsr	pc,relarg
	tst	opointer
	beq	2f
1:
	mov	opointer,r1
	jsr	pc,rewind
1:
	jsr	pc,getword
	bes	1f
	mov	r0,r1
	jsr	pc,release
	mov	opointer,r1
	br	1b
1:
	mov	opointer,r1
	jsr	pc,release
	tst	ipointer
	beq	2f
	mov	ipointer,r1
	jsr	pc,release
	tst	curly
	beq	2f
	jsr	pc,relcurl
	tst	scr
	beq	2f
	mov	scr,r1
	jsr	pc,release
2:
	jsr	pc,flush
	jsr	pc,whead
	sys	exit
letter:	.=.+2
arg:	.=.+20.
arge:	0
argf:	.=.+2
switch:	.=.+2
curly:	.=.+2
curlyf:	.=.+2
ch:	.=.+2
opointer:	.=.+2
ipointer:	.=.+2
scr:	.=.+2
iflag:	0
tflag:	0
end:
